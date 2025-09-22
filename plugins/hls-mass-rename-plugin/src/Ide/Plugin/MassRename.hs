{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted function" #-}
{-# HLINT ignore "Use fewer imports" #-}

module Ide.Plugin.MassRename (descriptor, E.Log) where

import           Control.Monad
import           Data.Maybe
import           Development.IDE                       (Recorder, WithPriority)
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import qualified Development.IDE.GHC.ExactPrint        as E
import           Development.IDE.Plugin.CodeAction
import           Development.IDE.Types.Location
import qualified Ide.Plugin.Rename as Rename
import           Ide.Types
import           Options.Applicative
import qualified System.Directory.Extra                   as IO
import           Control.Monad.Extra                      (concatMapM)
import           Data.List.Extra                          (isPrefixOf, nubOrd,
                                                           partition, split, sort)
import           System.FilePath                          (takeExtension,
                                                           takeFileName)
import qualified Development.IDE.GHC.Compat as GHC
-- import Debug.Trace
import Control.Monad.Except (runExceptT, ExceptT)
import Data.Either (fromRight)
import Ide.Plugin.Error (getNormalizedFilePathE, PluginError)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Development.IDE.Core.PluginUtils (runActionE, useE)
import Development.IDE.GHC.Compat (ParsedSource, NameAnn)
import Data.Text (Text)
import Development.IDE.GHC.Compat.Core (mkTcOcc)
import qualified Data.Text as T
import Data.Hashable (Hashable (hashWithSalt))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.HashSet as HS
import Data.List.NonEmpty.Extra (groupWith)
import Development.IDE.GHC.ExactPrint (GetAnnotatedParsedSource(..))
import Development.IDE.GHC.Compat.ExactPrint (exactPrint)
import Data.Algorithm.DiffContext (getContextDiff, prettyContextDiff)

import qualified Text.PrettyPrint.HughesPJ as P
import Development.IDE.GHC.Compat (OccName)
import Development.IDE.GHC.Compat (AnnListItem)
import Development.IDE.GHC.Compat (GenLocated(L))
import Development.IDE.GHC.Compat (SrcSpan)
import Development.IDE.GHC.Error (srcSpanToLocation)
import Debug.Trace
import qualified Data.List as List
import qualified Development.IDE.Spans.LocalBindings as LocalBindings
import qualified Data.HashMap.Strict as HashMap
import Development.IDE.Core.OfInterest (setFilesOfInterest)
import Generics.SYB (mkT , extT , Data , gmapT, ext2T, everywhere)
import System.Environment (lookupEnv)
import qualified Data.Text.IO as T
import GHC.Iface.Ext.Types (HieAST(..), NodeInfo(..), SourcedNodeInfo(..), HieASTs(..))
import qualified Data.Map as Map
import Language.Haskell.Syntax.Basic qualified as GHC
import Language.Haskell.Syntax.Expr qualified as GHC
import GHC.Data.FastString qualified as GHC

descriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder pluginId = mkExactprintPluginDescriptor recorder $
    (defaultPluginDescriptor pluginId "Rename all fields of a record")
        { pluginCli = Just exampleCli
        }

type TypeMap = Map.Map GHC.RealSrcSpan [GHC.Type]

exampleCli :: ParserInfo (IdeCommand IdeState)
exampleCli = info (IdeCommand . go <$> fileArg) mempty
    where

    fileArg = many (argument str (metavar "FILES/DIRS..."))
    go argFiles ide = do
        files <- expandFiles (argFiles ++ ["." | null argFiles])
        -- LSP works with absolute file paths, so try and behave similarly
        absoluteFiles <- nubOrd <$> mapM IO.canonicalizePath files
        putStrLn $ "Found " ++ show (length absoluteFiles) ++ " files"

        let nfps = map toNormalizedFilePath' absoluteFiles

        -- Is this necessary?
        -- Without this we get warnings when typechecking ("Typechecked a file which is not currently open in the editor")
        -- But with this, HLS does a lot of stuff and slows down
        setFilesOfInterest ide $ HashMap.fromList $ map (,OnDisk) nfps

        asts <- runAction "GetHieAst" ide $ uses GetHieAst nfps
        typeMaps :: Map.Map NormalizedFilePath TypeMap <- fmap Map.fromList $ forM (zip nfps asts) \case
            (nfp, Just HAR{hieKind=HieFresh, hieAst}) -> do
                let typeMap = Map.fromListWith (<>) $ map (fmap (:[])) $ foldMap nodeTypes $ Map.elems $ getAsts hieAst
                putStrLn $ GHC.printWithoutUniques typeMap
                pure (nfp, typeMap)
            -- Just HAR{hieKind=HieFromDisk{}, hieAst} -> do
            --     putStrLn $ GHC.printWithoutUniques hieAst
            (nfp, _) -> do
                _ <- error $ "HIEAST not fresh: " <> show nfp
                pure (nfp, mempty)

        results <- runAction "GetModIface" ide $ uses GetModIface (map toNormalizedFilePath' absoluteFiles)
        let (succeeded, failed) = partition (isJust . fst) $ zip results absoluteFiles
        unless (null failed) $
            putStr $ unlines $ "Files that failed:" : map ((++) " * " . snd) failed

        let state = ide

        fmap (fromRight (error "plugin error")) $ runExceptT $ do
            let typesToRefactor = flip foldMap succeeded $ \case
                    (Just mod, fp) ->
                        fmap (toNormalizedFilePath' fp,) (findTypesToRefactor mod)
                    _ -> []

            let refactoredTypeNames = HS.fromList $ HashableName . (.name) . snd <$> typesToRefactor

            directOldNames <-
                    fmap concat $ forM typesToRefactor \(nfp, tr) -> do
                        liftIO $ putStrLn $ "Found datatype " <> GHC.printWithoutUniques tr.module_ <> "." <> GHC.printWithoutUniques tr.name <> " with fields " <> show (GHC.printWithoutUniques <$> tr.fieldNames)
                        pure $ (nfp,) <$> tr.fieldNames

            directRefs <- concat <$> mapM (\(nfp, name) -> Rename.refsAtName state nfp name) directOldNames

            {- References in HieDB are not necessarily transitive. With `NamedFieldPuns`, we can have
                indirect references through punned names. To find the transitive closure, we do a pass of
                the direct references to find the references for any punned names.
                See the `IndirectPuns` test for an example. -}
            indirectOldNames <- concat . filter ((>1) . length) <$>
                forM directRefs \ref -> do
                    (nfp, pos) <- Rename.locToFilePos ref
                    fmap (nfp,) <$> Rename.getNamesAtPos state nfp pos
            let indirectOldNamesFiltered = filter (isIndirectRef . snd) indirectOldNames
                   where
                     isIndirectRef n =
                        fieldNameToString n `HashSet.member` directStrings
                        && not (nameHashKey n `HashSet.member` directNames)
                     directStrings = HashSet.fromList $ map (fieldNameToString . snd) directOldNames
                     directNames = HashSet.fromList $ map (nameHashKey .  snd) directOldNames

            indirectRefs <- concat <$> mapM (\(nfp, name) -> Rename.refsAtName state nfp name) indirectOldNamesFiltered

            liftIO $ putStrLn $ "Num direct refs: " <> show (length directRefs)
            liftIO $ putStrLn $ "Num indirect refs: " <> show (length indirectRefs)

            let refs = HashSet.fromList $ directRefs <> indirectRefs

            forM_ (withPrevious $ sort $ HS.toList refs) \(prev, loc) -> do
                nfp <- getNormalizedFilePathE loc._uri
                fileContents <- liftIO $ readFile (fromNormalizedFilePath nfp)
                when (Just loc._uri /= ((._uri) <$> prev)) do
                    liftIO $ putStrLn $ "  " <> T.unpack (getUri loc._uri)
                liftIO $ putStrLn $ "  " <> (lines fileContents !! fromIntegral loc._range._start._line)
                liftIO $ putStrLn $ "  " <> replicate (fromIntegral loc._range._start._character) ' '
                        <> replicate (fromIntegral (loc._range._end._character - loc._range._start._character)) '^'

            -- Perform rename
            let newName = rewriteOccName stripLensPrefix
                stripLensPrefix ('_':xs) = xs
                stripLensPrefix xs = xs
                filesRefs = collectWith (._uri) refs
                getFileEdit (uri, locations) = do
                    liftIO $ putStrLn $ T.unpack (getUri uri) <> ": " <> show (ppLoc <$> HS.toList locations)
                    nfp <- getNormalizedFilePathE uri
                    let typeMap = fromMaybe mempty $ Map.lookup nfp typeMaps
                    !x <- getSrcEdit ide uri (\lb ->
                        replaceRefs newName locations lb .
                        replaceFieldAccesses stripLensPrefix refactoredTypeNames typeMap)
                    pure x
            fileEdits <- mapM getFileEdit filesRefs

            liftIO $ putStrLn "DIFF:"
            forM_ fileEdits \edit -> do
                liftIO $ print $ prettyContextDiff (P.text $ T.unpack $ getUri edit.uri) (P.text $ T.unpack $ getUri edit.uri) (P.text . T.unpack) $
                    getContextDiff 1 (T.lines edit.before) (T.lines edit.after)

            shouldApply <- (== Just "1") <$> liftIO (lookupEnv "APPLY")
            when shouldApply do
                forM_ fileEdits \edit -> do
                    nfp <- getNormalizedFilePathE edit.uri
                    liftIO $ T.writeFile (fromNormalizedFilePath nfp) edit.after

rewriteOccName :: (String -> String) -> OccName -> OccName
rewriteOccName fn = mkTcOcc . fn . GHC.occNameString

newtype HashableName = HashableName { unHashableName :: GHC.Name }
    deriving (Eq)

instance Hashable HashableName where
    hashWithSalt salt = hashWithSalt salt . nameHashKey . unHashableName

nameHashKey :: GHC.Name -> Int
nameHashKey = GHC.getKey . GHC.nameUnique

nodeTypes :: HieAST a -> [(GHC.RealSrcSpan, a)]
nodeTypes node = local <> foldMap nodeTypes node.nodeChildren
    where
    local = do
        nodeInfo <- Map.elems (getSourcedNodeInfo node.sourcedNodeInfo)
        ty <- nodeInfo.nodeType
        pure (node.nodeSpan, ty)

ppLoc :: Location -> String
ppLoc loc = show (loc._range._start._line + 1) <> ":" <> show loc._range._start._character <> "-" <> show loc._range._end._character

ppLocWithFileName :: Location -> String
ppLocWithFileName loc = (T.unpack $ T.intercalate "/" $ untilSrc $ T.splitOn "/" $ getUri loc._uri) <> ":" <> ppLoc loc
    where untilSrc = dropWhile (/= "src")

-- | Whether we're inside a record field label.
--
-- Why is this needed? We report name shadowing conflicts arising from a rename.
-- However, reprting all `RdrName`s produces false positives in record field
-- labels, so we use this "traversal mode" to know whether to filter out the
-- conflict.
data Mode = Default | InRecordField deriving (Eq, Show)

-- | Replace names at every given `Location` (in a given `ParsedSource`) with a given new name.
replaceFieldAccesses ::
    (String -> String) ->
    HashSet HashableName -> -- ^ names of record types to refactor
    TypeMap ->
    ParsedSource ->
    ParsedSource
replaceFieldAccesses newName typesToRefactor typeMap = everywhere (mkT replaceExpr)
    where
    rewriteFieldLabelString = GHC.FieldLabelString . GHC.mkFastString . newName . GHC.unpackFS . GHC.field_label

    rewriteRdrName = \case
        GHC.Unqual nm -> GHC.Unqual (rewriteOccName newName nm)
        GHC.Qual x nm -> GHC.Qual x (rewriteOccName newName nm)
        GHC.Orig{} -> error "Orig RdrName should not happen here"
        GHC.Exact{} -> error "Exact RdrName should not happen here"

    replaceExpr :: GHC.HsExpr GHC.GhcPs -> GHC.HsExpr GHC.GhcPs
    replaceExpr = \case
        x@GHC.HsGetField { GHC.gf_expr = L srcSpan _, GHC.gf_field = L gfSpan gf_field@(GHC.DotFieldOcc { GHC.dfoLabel = label }) }
            | GHC.RealSrcSpan recordExprLoc _ <- GHC.locA srcSpan
            , Just (ty:_) <- Map.lookup recordExprLoc typeMap
            , GHC.TyConApp tyCon _ <- ty
            , HS.member (HashableName (GHC.getName tyCon)) typesToRefactor
            ->
                    trace ("record lookup " <> GHC.printWithoutUniques label <> " at type " <> GHC.printWithoutUniques ty)
                    x { GHC.gf_field = L gfSpan (gf_field { GHC.dfoLabel = rewriteFieldLabelString <$> label }) }

        x@GHC.RecordUpd { GHC.rupd_expr = L srcSpan _, GHC.rupd_flds = fields }
            | GHC.RealSrcSpan recordExprLoc _ <- GHC.locA srcSpan
            , Just (ty:_) <- Map.lookup recordExprLoc typeMap
            , GHC.TyConApp tyCon _ <- ty
            , HS.member (HashableName (GHC.getName tyCon)) typesToRefactor
            ->
                    let updatedFields =
                            case fields of
                                Right _fields' ->
                                    error ("overloaded record update at type " <> GHC.printWithoutUniques ty <> "@" <> ppLocWithFileName (unsafeSrcSpanToLoc (GHC.locA srcSpan)))
                                    -- Right (map (fmap
                                    --     (\field@GHC.HsFieldBind{GHC.hfbLHS = lhs} ->
                                    --         field { GHC.hfbLHS = fmap (\(GHC.FieldLabelStrings labels) ->
                                    --             GHC.FieldLabelStrings (fmap (fmap
                                    --                 (\dfo@GHC.DotFieldOcc{GHC.dfoLabel = label} -> dfo { GHC.dfoLabel = rewriteFieldLabelString <$> label })
                                    --             ) labels)) lhs })
                                    -- ) fields')
                                Left fields' ->
                                    trace ("normal record update at type " <> GHC.printWithoutUniques ty <> "@" <> ppLocWithFileName (unsafeSrcSpanToLoc (GHC.locA srcSpan))) $
                                    Left (map (fmap
                                        (\field@GHC.HsFieldBind{GHC.hfbLHS = lhs} ->
                                            field { GHC.hfbLHS = fmap (\case
                                                GHC.Ambiguous x rdrName -> GHC.Ambiguous x $ fmap rewriteRdrName rdrName
                                                GHC.Unambiguous x rdrName -> GHC.Unambiguous x $ fmap rewriteRdrName rdrName
                                            ) lhs })
                                    ) fields')

                    in x { GHC.rupd_flds = updatedFields }

        x -> x

-- | Replace names at every given `Location` (in a given `ParsedSource`) with a given new name.
replaceRefs ::
    (OccName -> OccName) ->
    HashSet Location ->
    LocalBindings.Bindings ->
    ParsedSource ->
    ParsedSource
replaceRefs newName refs lb = go Default
    where
        go :: forall a. Data a => Mode -> a -> a
        go mode =
            (gmapT (go mode) . (mkT (replaceLoc @AnnListItem mode) `extT` replaceLoc @NameAnn mode))
            `ext2T` goHsFieldBind mode

        goHsFieldBind mode (GHC.HsFieldBind ann lhs rhs pun)
            = GHC.HsFieldBind (go mode ann) (go InRecordField lhs) (go mode rhs) (go mode pun)

        replaceLoc :: forall an. Mode -> GHC.LocatedAn an GHC.RdrName -> GHC.LocatedAn an GHC.RdrName
--       replaceLoc (L srcSpan oldRdrName) | trace ("replace? " <> GHC.occNameString (GHC.rdrNameOcc oldRdrName)
--           <> " -> " <> show (isRef (GHC.locA srcSpan))) False = undefined
        replaceLoc mode (L srcSpan oldRdrName)
            | isRef (GHC.locA srcSpan) =
                let newName' = newName (GHC.rdrNameOcc oldRdrName)
                    !_ | GHC.RealSrcSpan realSpan _ <- GHC.locA srcSpan
                       , scope <- fst <$> LocalBindings.getLocalScope lb realSpan
                       , conflicts <- filter ((== GHC.occNameFS newName') . GHC.occNameFS . GHC.nameOccName) scope
                       , not (null conflicts)
                       , mode /= InRecordField
                       = trace ("CONFLICT: " <> GHC.printWithoutUniques conflicts <> " at " <> ppLocWithFileName (unsafeSrcSpanToLoc (GHC.locA srcSpan))) ()
                       | otherwise = ()
                in L srcSpan $ replace oldRdrName newName'
        replaceLoc _ lOldRdrName = lOldRdrName
        replace :: GHC.RdrName -> GHC.OccName -> GHC.RdrName
        replace (GHC.Qual modName _) newName' = GHC.Qual modName newName'
        replace _                    newName' = GHC.Unqual newName'

        isRef :: GHC.SrcSpan -> Bool
        isRef = (`HS.member` refs) . unsafeSrcSpanToLoc

unsafeSrcSpanToLoc :: SrcSpan -> Location
unsafeSrcSpanToLoc srcSpan =
    case srcSpanToLocation srcSpan of
        Nothing       -> error "Invalid conversion from UnhelpfulSpan to Location"
        Just location -> location

collectWith :: (Hashable a, Ord b) => (a -> b) -> HashSet a -> [(b, HashSet a)]
collectWith f = map (\(a :| as) -> (f a, HS.fromList (a:as))) . groupWith f . List.sortOn f . HS.toList

data FileEdit = FileEdit
    { uri :: Uri
    , before :: Text
    , after :: Text
    } deriving (Show)

-- Nicked from Rename plugin, but we're not using WorkspaceEdit since we're not
-- in a LSP environment.
-- We also provide local Bindings to the callback.
getSrcEdit ::
    MonadIO m =>
    IdeState ->
    Uri ->
    (LocalBindings.Bindings -> ParsedSource -> ParsedSource) ->
    ExceptT PluginError m FileEdit
getSrcEdit state uri updatePs = do
    nfp <- getNormalizedFilePathE uri
    annAst <- runActionE "Rename.GetAnnotatedParsedSource" state
        (useE GetAnnotatedParsedSource nfp)
    HAR{refMap=originalRefMap, hieKind} <- runActionE "Rename.GetHieAst" state $ useE GetHieAst nfp
    let refMap =
            case hieKind of
                HieFromDisk{} -> [] <$ originalRefMap
                HieFresh{} -> originalRefMap
    let ps = annAst
        src = T.pack $ exactPrint ps
        res = T.pack $ exactPrint (updatePs (LocalBindings.bindings refMap) ps)
    pure $ FileEdit
        { uri = uri
        , before = src
        , after = res
        }

withPrevious :: [a] -> [(Maybe a, a)]
withPrevious xs = zip (Nothing : map Just xs) xs

data TypeToRefactor = TypeToRefactor
    { module_ :: GHC.ModuleName
    , name :: GHC.Name
    , fieldNames :: [GHC.Name]
    }

findTypesToRefactor :: HiFileResult -> [TypeToRefactor]
findTypesToRefactor HiFileResult{hirModIface=modIface} =
    flip mapMaybe (GHC.mi_decls modIface) \decl ->
        case snd decl of
            GHC.IfaceData { GHC.ifName = nm, GHC.ifCons = GHC.IfDataTyCon _ constructors } -> do
                let fieldNames = concatMap getFieldNames constructors
                    getFieldNames = map GHC.flSelector . GHC.ifConFields

                let hasLensPrefix fieldName =
                        case fieldNameToString fieldName of
                            '_' : _ -> True
                            _ -> False
                --traceM $ show $ nameToString <$> fieldNames
                guard (not $ null fieldNames)
                guard (all hasLensPrefix fieldNames)
                pure TypeToRefactor
                    { module_ = GHC.moduleName $ GHC.mi_module modIface
                    , name = nm
                    , fieldNames
                    }
            _ -> Nothing

fieldNameToString :: GHC.Name -> String
fieldNameToString n =
    let ns = GHC.occNameString $ GHC.nameOccName n
    in case split (==':') ns of
        ["$sel", fieldName, _] -> fieldName
        _ -> ns

expandFiles :: [FilePath] -> IO [FilePath]
expandFiles = concatMapM $ \x -> do
    b <- IO.doesFileExist x
    if b
        then return [x]
        else do
            let recurse "." = True
                recurse y | "." `isPrefixOf` takeFileName y = False -- skip .git etc
                recurse y = takeFileName y `notElem` ["dist", "dist-newstyle"] -- cabal directories
            files <- filter (\y -> takeExtension y `elem` [".hs", ".lhs"]) <$> IO.listFilesInside (return . recurse) x
            when (null files) $
                fail $ "Couldn't find any .hs/.lhs files inside directory: " ++ x
            return files

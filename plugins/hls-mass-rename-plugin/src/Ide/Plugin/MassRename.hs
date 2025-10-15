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
                                                           takeFileName, takeDirectory, (</>))
import qualified Development.IDE.GHC.Compat as GHC
import Debug.Trace
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
import Generics.SYB (mkT , extT , Data , gmapT, ext2T, everywhere, everything, mkQ)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.IO as T
import GHC.Iface.Ext.Types (HieAST(..), NodeInfo(..), SourcedNodeInfo(..), HieASTs(..))
import qualified Data.Map as Map
import Language.Haskell.Syntax.Basic qualified as GHC
import GHC.Data.FastString qualified as GHC
import GHC.Parser.Annotation (EpAnn(EpAnnNotUsed, EpAnn), TrailingAnn(AddCommaAnn), AnnListItem(..), ann, AddEpAnn(..), AnnKeywordId(..), spanAsAnchor, emptyComments)
import qualified Data.Set as Set
import Data.Set (Set)
import Development.IDE.GHC.ExactPrint (setPrecedingLines, epl)
import Control.Lens (_last, over)
import GHC.Types.PkgQual (RawPkgQual(NoRawPkgQual))

descriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder pluginId = mkExactprintPluginDescriptor recorder $
    (defaultPluginDescriptor pluginId "Rename all fields of a record")
        { pluginCli = Just exampleCli
        }

type TypeMap = Map.Map GHC.RealSrcSpan [GHC.Type]

exampleCli :: ParserInfo (IdeCommand IdeState)
exampleCli = info (IdeCommand . go <$> parser) mempty
    where
    parser = (,)
        <$> some (strOption (long "scan" <> metavar "FILES/DIRS" <> help "Files/directories to scan for datatypes with lens-prefixed fields"))
        <*> some (strOption (long "rewrite" <> metavar "FILES/DIRS" <> help "Files/directories to rewrite"))
    go (scanArgs, rewriteArgs) ide = do
        -- Scan files: user-provided paths (to determine which types to refactor)
        scanFiles <- expandFiles scanArgs
        absoluteScanFiles <- nubOrd <$> mapM IO.canonicalizePath scanFiles
        putStrLn $ "Scanning " ++ show (length absoluteScanFiles) ++ " files for types to refactor"

        -- Rewrite files: user-specified files to transform
        allRewriteFiles <- expandFiles rewriteArgs
        absoluteRewriteFiles <- nubOrd <$> mapM IO.canonicalizePath allRewriteFiles
        putStrLn $ "Rewriting " ++ show (length absoluteRewriteFiles) ++ " files"

        -- Build HIE ASTs for all rewrite files
        let allNfps = map toNormalizedFilePath' absoluteRewriteFiles
        setFilesOfInterest ide $ HashMap.fromList $ map (,OnDisk) allNfps

        asts <- runAction "GetHieAst" ide $ uses GetHieAst allNfps
        -- Keep all loaded HIE ASTs for cross-file reference search
        let loadedHieAsts :: [HieAstResult]
            loadedHieAsts = catMaybes asts

        typeMaps :: Map.Map NormalizedFilePath TypeMap <- fmap Map.fromList $ forM (zip allNfps asts) \case
            (nfp, Just HAR{hieKind=HieFresh, hieAst}) -> do
                let typeMap = Map.fromListWith (<>) $ map (fmap (:[])) $ foldMap nodeTypes $ Map.elems $ getAsts hieAst
                pure (nfp, typeMap)
            (nfp, _) -> do
                -- Warn but don't error - file might not have HIE info
                putStrLn $ "Warning: No fresh HIE for " ++ show nfp ++ ", using empty typeMap"
                pure (nfp, mempty)

        -- Get ModIfaces for scan files to determine which types to refactor
        let scanNfps = map toNormalizedFilePath' absoluteScanFiles
        allResults <- runAction "GetModIface" ide $ uses GetModIface scanNfps
        let scanResults = zip allResults absoluteScanFiles
        let (succeeded, failed) = partition (isJust . fst) scanResults
        unless (null failed) $
            putStr $ unlines $ "Files that failed to get ModIface:" : map ((++) " * " . snd) failed

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

            -- Find references across all loaded HIE ASTs directly (bypassing HieDb)
            let refsAtNameInAsts :: GHC.Name -> [Location]
                refsAtNameInAsts name = concatMap (Rename.nameLocs name) loadedHieAsts

            let directRefs = concatMap (refsAtNameInAsts . snd) directOldNames

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

            let indirectRefs = concatMap (refsAtNameInAsts . snd) indirectOldNamesFiltered

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
                -- Create a map from URI to reference locations for efficient lookup
                refsMap = Map.fromList $ collectWith (._uri) refs

                -- Process ALL files from typeMaps, applying all transformations
                -- (not just files with HIE references)
                getFileEdit (nfp, typeMap) = do
                    let uri = fromNormalizedUri $ filePathToUri' nfp
                    let locations = fromMaybe HS.empty $ Map.lookup uri refsMap
                    when (not $ HS.null locations) $
                        liftIO $ putStrLn $ T.unpack (getUri uri) <> ": " <> show (ppLoc <$> HS.toList locations)
                    !x <- getSrcEdit ide uri (\lb ->
                        addMissingConstructorImports typeMap .
                        removeUnprefixFieldsCalls refactoredTypeNames .
                        replaceRefs newName locations lb .
                        replaceFieldAccesses stripLensPrefix refactoredTypeNames typeMap)
                    pure x

            allEdits <- mapM getFileEdit (Map.toList typeMaps)

            liftIO $ putStrLn "DIFF:"
            forM_ allEdits \edit -> do
                liftIO $ print $ prettyContextDiff (P.text $ T.unpack $ getUri edit.uri) (P.text $ T.unpack $ getUri edit.uri) (P.text . T.unpack) $
                    getContextDiff 1 (T.lines edit.before) (T.lines edit.after)

            shouldApply <- (== Just "1") <$> liftIO (lookupEnv "APPLY")
            when shouldApply do
                forM_ allEdits \edit -> do
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
            -> let
                debugInfo = case GHC.locA srcSpan of
                    GHC.RealSrcSpan recordExprLoc _ ->
                        let typeMapLookup = Map.lookup recordExprLoc typeMap
                            (typeStr, tyConStr, matches) = case typeMapLookup of
                                Just (ty:_) | GHC.TyConApp tyCon _ <- ty ->
                                    let tyName = GHC.getName tyCon
                                        matchesTy = HS.member (HashableName tyName) typesToRefactor
                                    in (GHC.printWithoutUniques ty, GHC.printWithoutUniques tyName, matchesTy)
                                Just (ty:_) -> (GHC.printWithoutUniques ty, "NO_TYCONAPP", False)
                                Just [] -> ("EMPTY_LIST", "NO", False)
                                Nothing -> ("NOT_FOUND", "NO", False)
                            typeMapStr = if isJust typeMapLookup then "FOUND" else "NOT_FOUND"
                            matchStr = if matches then "YES" else "NO"
                        in Just ("FIELD_ACCESS: " <> GHC.printWithoutUniques recordExprLoc <> " | " <> GHC.printWithoutUniques label <> " | TypeMap=" <> typeMapStr <> " | Type=" <> typeStr <> " | TyConApp=" <> tyConStr <> " | Match=" <> matchStr)
                    _ -> Nothing
                !_ = case debugInfo of
                    Just msg -> trace msg ()
                    Nothing -> ()
               in case GHC.locA srcSpan of
                    GHC.RealSrcSpan recordExprLoc _
                        | Just (ty:_) <- Map.lookup recordExprLoc typeMap
                        , GHC.TyConApp tyCon _ <- ty
                        , HS.member (HashableName (GHC.getName tyCon)) typesToRefactor
                        -> x { GHC.gf_field = L gfSpan (gf_field { GHC.dfoLabel = rewriteFieldLabelString <$> label }) }
                    _ -> x

        x@GHC.RecordUpd { GHC.rupd_expr = L srcSpan _, GHC.rupd_flds = fields }
            -> let
                getFieldNames = case fields of
                    Left fields' -> show (length fields') <> " fields"
                    Right _fields' -> "overloaded fields"
                debugInfo = case GHC.locA srcSpan of
                    GHC.RealSrcSpan recordExprLoc _ ->
                        let typeMapLookup = Map.lookup recordExprLoc typeMap
                            (typeStr, tyConStr, matches) = case typeMapLookup of
                                Just (ty:_) | GHC.TyConApp tyCon _ <- ty ->
                                    let tyName = GHC.getName tyCon
                                        matchesTy = HS.member (HashableName tyName) typesToRefactor
                                    in (GHC.printWithoutUniques ty, GHC.printWithoutUniques tyName, matchesTy)
                                Just (ty:_) -> (GHC.printWithoutUniques ty, "NO_TYCONAPP", False)
                                Just [] -> ("EMPTY_LIST", "NO", False)
                                Nothing -> ("NOT_FOUND", "NO", False)
                            typeMapStr = if isJust typeMapLookup then "FOUND" else "NOT_FOUND"
                            matchStr = if matches then "YES" else "NO"
                        in Just ("RECORD_UPDATE: " <> GHC.printWithoutUniques recordExprLoc <> " | " <> getFieldNames <> " | TypeMap=" <> typeMapStr <> " | Type=" <> typeStr <> " | TyConApp=" <> tyConStr <> " | Match=" <> matchStr)
                    _ -> Nothing
                !_ = case debugInfo of
                    Just msg -> trace msg ()
                    Nothing -> ()
               in case GHC.locA srcSpan of
                    GHC.RealSrcSpan recordExprLoc _
                        | Just (ty:_) <- Map.lookup recordExprLoc typeMap
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
                                            Left (map (fmap
                                                (\field@GHC.HsFieldBind{GHC.hfbLHS = lhs} ->
                                                    field { GHC.hfbLHS = fmap (\case
                                                        GHC.Ambiguous x rdrName -> GHC.Ambiguous x $ fmap rewriteRdrName rdrName
                                                        GHC.Unambiguous x rdrName -> GHC.Unambiguous x $ fmap rewriteRdrName rdrName
                                                    ) lhs })
                                            ) fields')

                            in x { GHC.rupd_flds = updatedFields }
                    _ -> x

        x -> x

-- | Extract the module name from a Type (if it's a TyConApp)
getTyConModule :: GHC.Type -> Maybe (GHC.ModuleName, GHC.Name)
getTyConModule (GHC.TyConApp tyCon _) =
    case GHC.nameModule_maybe (GHC.getName tyCon) of
        Just mod -> Just (GHC.moduleName mod, GHC.getName tyCon)
        Nothing -> Nothing
getTyConModule _ = Nothing

-- | Collect all record types that are used in field accesses in the given ParsedSource
--   These types will need their constructors to be imported for OverloadedRecordDot to work
collectFieldAccessTypes :: TypeMap -> ParsedSource -> Set (GHC.ModuleName, GHC.Name)
collectFieldAccessTypes typeMap ps =
    let spans = collectFieldAccesses ps
        !_ = trace ("COLLECT_FIELD_ACCESS_TYPES: Found " <> show (length spans) <> " field access spans") ()
        types = mapMaybe extractType spans
        result = Set.fromList types
        !_ = trace ("COLLECT_FIELD_ACCESS_TYPES: Extracted " <> show (Set.size result) <> " unique types: " <> show (map (\(m, n) -> GHC.printWithoutUniques m <> "." <> GHC.printWithoutUniques n) (Set.toList result))) ()
    in result
  where
    extractType :: GHC.RealSrcSpan -> Maybe (GHC.ModuleName, GHC.Name)
    extractType srcSpan =
        let lookupResult = Map.lookup srcSpan typeMap
            result = case lookupResult of
                Nothing -> Nothing
                Just [] -> Nothing
                Just (ty:_) -> getTyConModule ty
            !_ = trace ("COLLECT_SPAN: " <> GHC.printWithoutUniques srcSpan <> " | TypeMap=" <> (if isJust lookupResult then "FOUND" else "NOT_FOUND") <> " | Result=" <> maybe "NONE" (\(m, n) -> GHC.printWithoutUniques m <> "." <> GHC.printWithoutUniques n) result) ()
        in result

    collectFieldAccesses :: Data a => a -> [GHC.RealSrcSpan]
    collectFieldAccesses = everything (++) (mkQ [] getFieldAccessSpan)

    getFieldAccessSpan :: GHC.HsExpr GHC.GhcPs -> [GHC.RealSrcSpan]
    getFieldAccessSpan = \case
        -- Field access: r.field
        GHC.HsGetField { GHC.gf_expr = L srcSpan _ }
            | GHC.RealSrcSpan recordExprLoc _ <- GHC.locA srcSpan
            -> [recordExprLoc]
        -- Field projection: (.field)
        GHC.HsProjection { GHC.proj_flds = _ }
            -> []  -- TODO: handle projections if needed
        _ -> []

-- | Extract OccName from IEWrappedName
getIEName :: GHC.IEWrappedName GHC.GhcPs -> GHC.OccName
getIEName (GHC.IEName _ (GHC.L _ rdrName)) = GHC.rdrNameOcc rdrName
getIEName (GHC.IEPattern _ (GHC.L _ rdrName)) = GHC.rdrNameOcc rdrName
getIEName (GHC.IEType _ (GHC.L _ rdrName)) = GHC.rdrNameOcc rdrName

-- | Check if a type constructor is accessible (i.e., constructor is imported)
--   from the given import declarations
hasConstructorAccess :: GHC.ModuleName -> GHC.Name -> [GHC.LImportDecl GHC.GhcPs] -> Bool
hasConstructorAccess targetModule targetName imports =
    let result = any checkImport imports
        !_ = trace ("HAS_CONSTRUCTOR_ACCESS: " <> GHC.printWithoutUniques targetModule <> "." <> GHC.printWithoutUniques targetName <> " | Result=" <> if result then "YES" else "NO") ()
    in result
  where
    checkImport :: GHC.LImportDecl GHC.GhcPs -> Bool
    checkImport (GHC.L _ imp)
        | GHC.unLoc (GHC.ideclName imp) == targetModule =
            let result = case GHC.ideclImportList imp of
                    -- No import list means all constructors are accessible
                    -- This includes both "import M" and "import qualified M" - both make constructors accessible
                    Nothing -> True
                    -- Check if type with constructors is in import list
                    Just (GHC.Exactly, limports) ->
                        any (hasTypeConstructor targetName) (map GHC.unLoc (GHC.unLoc limports))
                    Just (GHC.EverythingBut, _) -> False  -- Hiding list - too complex
                !_ = trace ("  CHECK_IMPORT: module=" <> GHC.printWithoutUniques (GHC.unLoc (GHC.ideclName imp)) <> " | hasImportList=" <> show (isJust (GHC.ideclImportList imp)) <> " | result=" <> if result then "YES" else "NO") ()
            in result
        | otherwise = False

    hasTypeConstructor :: GHC.Name -> GHC.IE GHC.GhcPs -> Bool
    hasTypeConstructor name ie = case ie of
        -- Type with all constructors: Type(..)
        GHC.IEThingAll _ (GHC.L _ ieName) ->
            getIEName ieName == GHC.nameOccName name
        -- Type with specific constructors: Type(Con1, Con2)
        GHC.IEThingWith _ (GHC.L _ ieName) _ _ ->
            getIEName ieName == GHC.nameOccName name
        -- Type without constructors: Type
        GHC.IEThingAbs _ _ -> False
        -- Variable import
        GHC.IEVar _ _ -> False
        _ -> False

-- | Add missing constructor imports to a ParsedSource
--   This is needed for OverloadedRecordDot to work after renaming fields
addMissingConstructorImports :: TypeMap -> ParsedSource -> ParsedSource
addMissingConstructorImports typeMap ps@(GHC.L loc hsModule) =
    let accessedTypes = collectFieldAccessTypes typeMap ps
        !_ = trace ("ADD_MISSING_CONSTRUCTOR_IMPORTS: accessedTypes=" <> show (map (\(m, n) -> GHC.printWithoutUniques m <> "." <> GHC.printWithoutUniques n) (Set.toList accessedTypes))) ()
        imports = GHC.hsmodImports hsModule

        -- Get current module name to prevent self-imports
        currentModuleName = fmap GHC.unLoc (GHC.hsmodName hsModule)
        !_ = trace ("ADD_MISSING_CONSTRUCTOR_IMPORTS: currentModule=" <> maybe "NONE" GHC.printWithoutUniques currentModuleName) ()

        -- Find types that need constructor imports
        typesNeedingImports = Set.filter
            (\(modName, tyName) -> not $ hasConstructorAccess modName tyName imports)
            accessedTypes
        !_ = trace ("ADD_MISSING_CONSTRUCTOR_IMPORTS: typesNeedingImports=" <> show (map (\(m, n) -> GHC.printWithoutUniques m <> "." <> GHC.printWithoutUniques n) (Set.toList typesNeedingImports))) ()

        -- Modify imports to add constructors
        modifiedImports = modifyImports currentModuleName (Set.toList typesNeedingImports) imports
        !_ = if Set.null typesNeedingImports
            then trace "ADD_MISSING_CONSTRUCTOR_IMPORTS: No changes needed" ()
            else trace ("ADD_MISSING_CONSTRUCTOR_IMPORTS: Modifying imports for " <> show (Set.size typesNeedingImports) <> " types") ()
    in if Set.null typesNeedingImports
        then ps  -- No changes needed
        else GHC.L loc (hsModule { GHC.hsmodImports = modifiedImports })

-- | Modify import declarations to add missing constructor imports
modifyImports :: Maybe GHC.ModuleName -> [(GHC.ModuleName, GHC.Name)] -> [GHC.LImportDecl GHC.GhcPs] -> [GHC.LImportDecl GHC.GhcPs]
modifyImports _ [] imports = imports
modifyImports currentModule typesToAdd imports =
    -- Two-phase approach:
    -- 1. Match by OccName (handles re-exports: if Account from Types5Internal is imported via Types5)
    -- 2. Match by module name (handles new imports: if MenuSection needs to be added to Types3 import)
    -- 3. Create new import lines for modules that aren't imported at all (excluding current module)
    let allTypeNames = map snd typesToAdd
        typesByModule = Map.fromListWith (++) [(mod, [name]) | (mod, name) <- typesToAdd]
        !_ = trace ("MODIFY_IMPORTS: allTypeNames=" <> show (map GHC.printWithoutUniques allTypeNames)) ()
        !_ = trace ("MODIFY_IMPORTS: typesByModule=" <> show (Map.mapKeys GHC.printWithoutUniques $ fmap (map GHC.printWithoutUniques) typesByModule)) ()

        -- Get set of all modules currently imported
        importedModules = Set.fromList $ map (GHC.unLoc . GHC.ideclName . GHC.unLoc) imports

        -- Collect all types that were handled by OccName matching in existing imports
        typesHandledByOccName = Set.fromList $ concatMap (\imp -> findTypesImportedByOccName allTypeNames (GHC.unLoc imp)) imports

        -- Find modules that need types but aren't imported yet
        -- Exclude:
        -- 1. Modules already imported
        -- 2. The current module (to prevent self-imports)
        -- 3. Types that were already handled by OccName matching in existing imports
        modulesNeedingImports = Map.map (\names -> filter (\n -> not $ n `Set.member` typesHandledByOccName) names) $
            Map.filterWithKey (\modName _ ->
                not (modName `Set.member` importedModules) &&  -- Not already imported
                Just modName /= currentModule                   -- Not the current module (prevent self-import)
            ) typesByModule
        -- Remove empty entries (all types were handled by OccName)
        modulesNeedingImportsFiltered = Map.filter (not . null) modulesNeedingImports

        -- Modify existing imports
        modifiedImports = map (modifyImport allTypeNames typesByModule) imports

        -- Create new import declarations for modules not yet imported
        newImports = map (createNewImport typesByModule) (Map.toList modulesNeedingImportsFiltered)

        !_ = if Map.null modulesNeedingImportsFiltered
            then trace "MODIFY_IMPORTS: No new imports needed" ()
            else trace ("MODIFY_IMPORTS: Creating " <> show (Map.size modulesNeedingImportsFiltered) <> " new import(s): " <> show (map GHC.printWithoutUniques (Map.keys modulesNeedingImportsFiltered))) ()
    in modifiedImports ++ newImports
  where
    modifyImport :: [GHC.Name] -> Map.Map GHC.ModuleName [GHC.Name] -> GHC.LImportDecl GHC.GhcPs -> GHC.LImportDecl GHC.GhcPs
    modifyImport allNames typesByModule (GHC.L loc imp) =
        let modName = GHC.unLoc $ GHC.ideclName imp
            -- Phase 1: Find types imported here by OccName (handles re-exports)
            typesByOccName = findTypesImportedByOccName allNames imp
            -- Phase 2: Find types that should be imported from this module but aren't imported yet
            typesFromThisModule = Map.findWithDefault [] modName typesByModule
            -- Filter out types already found by OccName match
            typesNotYetImported = filter (\n -> not (n `elem` typesByOccName)) typesFromThisModule
            -- Combine both
            allTypesToAdd = typesByOccName ++ typesNotYetImported
        in if null allTypesToAdd
            then GHC.L loc imp
            else
                let !_ = trace ("  MODIFY_IMPORT: module=" <> GHC.printWithoutUniques modName <> " | byOccName=" <> show (map GHC.printWithoutUniques typesByOccName) <> " | byModule=" <> show (map GHC.printWithoutUniques typesNotYetImported)) ()
                in GHC.L loc (addTypesToImport allTypesToAdd imp)

    -- Find which type Names from the list are imported by this import (matching by OccName)
    -- This allows handling re-exports: if Types5Internal.Account is re-exported by Types5,
    -- we'll match it against "import Types5 (Account)"
    findTypesImportedByOccName :: [GHC.Name] -> GHC.ImportDecl GHC.GhcPs -> [GHC.Name]
    findTypesImportedByOccName names imp =
        case GHC.ideclImportList imp of
            Nothing ->
                -- No import list - could be open import or qualified import
                -- Qualified imports: constructors already accessible qualified, no need to add
                -- Open imports: all names need constructors added
                if GHC.ideclQualified imp == GHC.NotQualified
                    then names  -- Open import: all names are imported and need constructors
                    else []     -- Qualified import: constructors already accessible qualified
            Just (GHC.Exactly, GHC.L _ limports) ->
                -- Check which names appear in the import list (by OccName)
                let importedOccNames = Set.fromList $ mapMaybe getImportedTypeName limports
                in filter (\n -> GHC.nameOccName n `Set.member` importedOccNames) names
            Just (GHC.EverythingBut, _) -> []  -- Hiding list - skip

    addTypesToImport :: [GHC.Name] -> GHC.ImportDecl GHC.GhcPs -> GHC.ImportDecl GHC.GhcPs
    addTypesToImport names imp =
        case GHC.ideclImportList imp of
            Nothing -> imp  -- Open import, nothing to do
            Just (GHC.Exactly, GHC.L loc limports) ->
                -- Explicit import list - transform IEThingAbs to IEThingAll, and add missing types
                let namesSet = Set.fromList names
                    -- Transform matching IEThingAbs to IEThingAll, keep others as-is
                    transformedImports = map (transformImport namesSet) limports
                    transformedList = map snd transformedImports
                    -- Find names that weren't already in the import list
                    importedNames = Set.fromList $ mapMaybe getImportedTypeName limports
                    missingNames = filter (\n -> not (GHC.nameOccName n `Set.member` importedNames)) names
                    -- Create new IEThingAll entries for missing names
                    newImports = map makeIEThingAll missingNames
                    -- Combine transformed and new imports
                    resultImports = if null newImports
                                    then transformedList
                                    else addImportsWithCommas transformedList newImports
                in imp { GHC.ideclImportList = Just (GHC.Exactly, GHC.L loc resultImports) }
            Just (GHC.EverythingBut, _) -> imp  -- Hiding list - skip for now (too complex)

    -- Get the OccName of a type being imported (for any IE variant)
    getImportedTypeName :: GHC.LIE GHC.GhcPs -> Maybe GHC.OccName
    getImportedTypeName (GHC.L _ ie) = case ie of
        GHC.IEThingAbs _ (GHC.L _ ieName) -> Just (getIEName ieName)
        GHC.IEThingAll _ (GHC.L _ ieName) -> Just (getIEName ieName)
        GHC.IEThingWith _ (GHC.L _ ieName) _ _ -> Just (getIEName ieName)
        _ -> Nothing

    -- Transform IEThingAbs to IEThingAll if it matches, return (wasTransformed, result)
    transformImport :: Set.Set GHC.Name -> GHC.LIE GHC.GhcPs -> (Bool, GHC.LIE GHC.GhcPs)
    transformImport namesSet limport@(GHC.L loc ie) = case ie of
        GHC.IEThingAbs ext lieName@(GHC.L _ ieName) ->
            let occName = getIEName ieName
                targetOccNames = map GHC.nameOccName (Set.toList namesSet)
                matches = occName `elem` targetOccNames
            in if matches
               then let -- Add (..) annotations to the extension
                        newExt = addDotDotAnnotations ext
                    in (True, GHC.L loc (GHC.IEThingAll newExt lieName))
               else (False, limport)
        _ -> (False, limport)

    -- Add (..) annotations to the EpAnn for IEThingAll
    addDotDotAnnotations :: EpAnn [AddEpAnn] -> EpAnn [AddEpAnn]
    addDotDotAnnotations EpAnnNotUsed = EpAnnNotUsed
    addDotDotAnnotations (EpAnn anchor anns comments) =
        let dotdotAnns = [ AddEpAnn AnnOpenP (epl 0)
                         , AddEpAnn AnnDotdot (epl 0)
                         , AddEpAnn AnnCloseP (epl 0)
                         ]
        in EpAnn anchor (anns ++ dotdotAnns) comments

    -- Check if an import entry is a type-only import (IEThingAbs) for one of the given names
    -- Note: This function is currently unused as we now handle transformations differently
    isTypeOnlyImport :: Set.Set GHC.Name -> GHC.LocatedAn AnnListItem (GHC.IE GHC.GhcPs) -> Bool
    isTypeOnlyImport namesSet (GHC.L _ ie) = case ie of
        GHC.IEThingAbs _ (GHC.L _ ieName) ->
            getIEName ieName `elem` map GHC.nameOccName (Set.toList namesSet)
        _ -> False

    -- Add new imports to existing list with proper comma annotations
    addImportsWithCommas :: [GHC.LocatedAn AnnListItem (GHC.IE GHC.GhcPs)]
                         -> [GHC.LocatedAn AnnListItem (GHC.IE GHC.GhcPs)]
                         -> [GHC.LocatedAn AnnListItem (GHC.IE GHC.GhcPs)]
    addImportsWithCommas [] newItems = newItems
    addImportsWithCommas existing [] = existing
    addImportsWithCommas existing newItems =
        let -- Add trailing comma to the last existing item if needed
            existingWithComma = addTrailingCommaToLast existing
            -- Add trailing commas to all new items except the last
            newItemsWithCommas = addTrailingCommasExceptLast newItems
            -- Add spacing to new items (SameLine 1 = 0 lines, 1 column)
            newItemsWithSpacing = map (\item -> setPrecedingLines item 0 1) newItemsWithCommas
        in existingWithComma ++ newItemsWithSpacing

    -- Add trailing comma to the last item in the list if not already present
    addTrailingCommaToLast :: [GHC.LocatedAn AnnListItem a] -> [GHC.LocatedAn AnnListItem a]
    addTrailingCommaToLast [] = []
    addTrailingCommaToLast items = over _last addCommaToItem items
      where
        addCommaToItem :: GHC.LocatedAn AnnListItem a -> GHC.LocatedAn AnnListItem a
        addCommaToItem (GHC.L srcAnn item) =
            let newAnn = case ann srcAnn of
                    EpAnn anchor (AnnListItem trailing) comments ->
                        -- Check if comma already exists
                        let hasComma = any isComma trailing
                            newTrailing = if hasComma then trailing else trailing ++ [AddCommaAnn (epl 0)]
                        in EpAnn anchor (AnnListItem newTrailing) comments
                    other -> other  -- EpAnnNotUsed or other cases
            in GHC.L (srcAnn { ann = newAnn }) item

        isComma (AddCommaAnn _) = True
        isComma _ = False

    -- Add trailing commas to all items except the last one
    addTrailingCommasExceptLast :: [GHC.LocatedAn AnnListItem a] -> [GHC.LocatedAn AnnListItem a]
    addTrailingCommasExceptLast [] = []
    addTrailingCommasExceptLast [x] = [x]  -- Last item gets no comma
    addTrailingCommasExceptLast items =
        let allButLast = init items
            lastItem = last items
        in map addCommaToItem allButLast ++ [lastItem]
      where
        addCommaToItem :: GHC.LocatedAn AnnListItem a -> GHC.LocatedAn AnnListItem a
        addCommaToItem (GHC.L srcAnn item) =
            let newAnn = case ann srcAnn of
                    EpAnn anchor (AnnListItem trailing) comments ->
                        -- Check if comma already exists
                        let hasComma = any isComma trailing
                            newTrailing = if hasComma then trailing else trailing ++ [AddCommaAnn (epl 0)]
                        in EpAnn anchor (AnnListItem newTrailing) comments
                    EpAnnNotUsed ->
                        -- Create a new EpAnn with just the comma
                        EpAnn (spanAsAnchor GHC.noSrcSpan) (AnnListItem [AddCommaAnn (epl 0)]) emptyComments
                    other -> other  -- Other cases
            in GHC.L (srcAnn { ann = newAnn }) item

        isComma (AddCommaAnn _) = True
        isComma _ = False

    makeIEThingAll :: GHC.Name -> GHC.LIE GHC.GhcPs
    makeIEThingAll name =
        let rdrName = GHC.nameRdrName name
            ieName = GHC.IEName GHC.noExtField (GHC.noLocA rdrName)
            -- Create annotations for (..)
            dotdotAnns = [ AddEpAnn AnnOpenP (epl 0)
                         , AddEpAnn AnnDotdot (epl 0)
                         , AddEpAnn AnnCloseP (epl 0)
                         ]
            ext = EpAnn (spanAsAnchor GHC.noSrcSpan) dotdotAnns emptyComments
        in GHC.noLocA (GHC.IEThingAll ext (GHC.noLocA ieName))

    -- Create a completely new import declaration for a module
    createNewImport :: Map.Map GHC.ModuleName [GHC.Name] -> (GHC.ModuleName, [GHC.Name]) -> GHC.LImportDecl GHC.GhcPs
    createNewImport typesByModule (modName, typeNames) =
        let -- Create import items for each type
            importItems = map makeIEThingAll typeNames
            -- Create the import declaration
            importDecl = GHC.ImportDecl
                { GHC.ideclExt = GHC.XImportDeclPass
                    { GHC.ideclAnn = EpAnnNotUsed
                    , GHC.ideclSourceText = GHC.NoSourceText
                    , GHC.ideclImplicit = False
                    }
                , GHC.ideclName = GHC.noLocA modName
                , GHC.ideclPkgQual = NoRawPkgQual
                , GHC.ideclSource = GHC.NotBoot
                , GHC.ideclSafe = False
                , GHC.ideclQualified = GHC.NotQualified
                , GHC.ideclAs = Nothing
                , GHC.ideclImportList = Just (GHC.Exactly, GHC.noLocA importItems)
                }
            -- Add proper spacing for the new import (1 line before, 0 columns)
            limportDecl = setPrecedingLines (GHC.noLocA importDecl) 1 0
            !_ = trace ("  CREATE_NEW_IMPORT: module=" <> GHC.printWithoutUniques modName <> " | types=" <> show (map GHC.printWithoutUniques typeNames)) ()
        in limportDecl

-- | Remove `unprefixFields ''TypeName` declarations for types being refactored
--   Since we're removing the field prefixes, the unprefixFields TH calls are no longer needed
removeUnprefixFieldsCalls :: HashSet HashableName -> ParsedSource -> ParsedSource
removeUnprefixFieldsCalls typesToRefactor (GHC.L loc hsModule) =
    let decls = GHC.hsmodDecls hsModule
        filteredDecls = filter (not . isUnprefixFieldsCallForRefactoredType) decls
    in GHC.L loc (hsModule { GHC.hsmodDecls = filteredDecls })
  where
    -- Check if a declaration is an unprefixFields call for a type we're refactoring
    isUnprefixFieldsCallForRefactoredType :: GHC.LHsDecl GHC.GhcPs -> Bool
    isUnprefixFieldsCallForRefactoredType (GHC.L _ decl) = case decl of
        GHC.SpliceD _ splice -> isSpliceForRefactoredType splice
        _ -> False

    isSpliceForRefactoredType :: GHC.SpliceDecl GHC.GhcPs -> Bool
    isSpliceForRefactoredType spliceDecl =
        -- Use SYB to traverse the splice and look for the pattern we need
        let hasUnprefixFieldsCall = everything (||) (mkQ False isUnprefixFieldsCall) spliceDecl
            hasRefactoredTypeName = everything (||) (mkQ False isRefactoredTypeName) spliceDecl
        in hasUnprefixFieldsCall && hasRefactoredTypeName

    -- Check if an expression is a call to unprefixFields
    isUnprefixFieldsCall :: GHC.HsExpr GHC.GhcPs -> Bool
    isUnprefixFieldsCall = \case
        GHC.HsVar _ (GHC.L _ rdrName) ->
            GHC.rdrNameOcc rdrName == GHC.mkVarOcc "unprefixFields"
        _ -> False

    -- Check if an RdrName refers to a type we're refactoring
    isRefactoredTypeName :: GHC.RdrName -> Bool
    isRefactoredTypeName = \case
        GHC.Exact name -> HashableName name `HS.member` typesToRefactor
        GHC.Unqual occName ->
            -- Check if any refactored type has a matching OccName
            any (\(HashableName name) -> GHC.nameOccName name == occName) (HS.toList typesToRefactor)
        _ -> False

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
        isRef srcSpan = case srcSpanToLocation srcSpan of
            Nothing -> False  -- UnhelpfulSpan can't be a reference
            Just location -> location `HS.member` refs

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

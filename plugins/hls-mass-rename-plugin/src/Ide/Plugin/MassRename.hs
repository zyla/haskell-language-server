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
        types = mapMaybe extractType spans
    in Set.fromList types
  where
    extractType :: GHC.RealSrcSpan -> Maybe (GHC.ModuleName, GHC.Name)
    extractType srcSpan =
        case Map.lookup srcSpan typeMap of
            Nothing -> Nothing
            Just [] -> Nothing
            Just (ty:_) -> getTyConModule ty

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
    any checkImport imports
  where
    checkImport :: GHC.LImportDecl GHC.GhcPs -> Bool
    checkImport (GHC.L _ imp)
        | GHC.unLoc (GHC.ideclName imp) == targetModule =
            case GHC.ideclImportList imp of
                -- No import list means everything is imported (if not qualified-only)
                Nothing -> GHC.ideclQualified imp /= GHC.QualifiedPre && GHC.ideclQualified imp /= GHC.QualifiedPost
                -- Check if type with constructors is in import list
                Just (GHC.Exactly, limports) ->
                    any (hasTypeConstructor targetName) (map GHC.unLoc (GHC.unLoc limports))
                Just (GHC.EverythingBut, _) -> False  -- Hiding list - too complex
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
        imports = GHC.hsmodImports hsModule

        -- Find types that need constructor imports
        typesNeedingImports = Set.filter
            (\(modName, tyName) -> not $ hasConstructorAccess modName tyName imports)
            accessedTypes

        -- Modify imports to add constructors
        modifiedImports = modifyImports (Set.toList typesNeedingImports) imports
    in if Set.null typesNeedingImports
        then ps  -- No changes needed
        else GHC.L loc (hsModule { GHC.hsmodImports = modifiedImports })

-- | Modify import declarations to add missing constructor imports
modifyImports :: [(GHC.ModuleName, GHC.Name)] -> [GHC.LImportDecl GHC.GhcPs] -> [GHC.LImportDecl GHC.GhcPs]
modifyImports [] imports = imports
modifyImports typesToAdd imports =
    -- Group types by module
    let typesByModule = Map.fromListWith (++) [(modName, [tyName]) | (modName, tyName) <- typesToAdd]
    in map (modifyImport typesByModule) imports
  where
    modifyImport :: Map.Map GHC.ModuleName [GHC.Name] -> GHC.LImportDecl GHC.GhcPs -> GHC.LImportDecl GHC.GhcPs
    modifyImport typeMap (GHC.L loc imp) =
        case Map.lookup (GHC.unLoc $ GHC.ideclName imp) typeMap of
            Nothing -> GHC.L loc imp
            Just names -> GHC.L loc (addTypesToImport names imp)

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

{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

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
import           Ide.Types
import           Options.Applicative
import qualified System.Directory.Extra                   as IO
import           Control.Monad.Extra                      (concatMapM)
import           Data.List.Extra                          (isPrefixOf, nubOrd,
                                                           partition)
import           System.FilePath                          (takeExtension,
                                                           takeFileName)
import qualified Development.IDE.GHC.Compat as GHC
import GHC.Types.SrcLoc (unLoc)

descriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder pluginId = mkExactprintPluginDescriptor recorder $
    (defaultPluginDescriptor pluginId "Rename all fields of a record")
        { pluginCli = Just exampleCli
        }


exampleCli :: ParserInfo (IdeCommand IdeState)
exampleCli = info (IdeCommand . go <$> fileArg) mempty
    where

    fileArg = many (argument str (metavar "FILES/DIRS..."))
    go argFiles ide = do
        files <- expandFiles (argFiles ++ ["." | null argFiles])
        -- LSP works with absolute file paths, so try and behave similarly
        absoluteFiles <- nubOrd <$> mapM IO.canonicalizePath files
        putStrLn $ "Found " ++ show (length absoluteFiles) ++ " files"

        -- TODO: is this necessary?
        --setFilesOfInterest ide $ HashMap.fromList $ map ((,OnDisk) . toNormalizedFilePath') absoluteFiles

        results <- runAction "GetParsedModule" ide $ uses GetParsedModule (map toNormalizedFilePath' absoluteFiles)
        let (_, failed) = partition fst $ zip (map isJust results) absoluteFiles
        when (failed /= []) $
            putStr $ unlines $ "Files that failed:" : map ((++) " * " . snd) failed

        forM_ (catMaybes results) \mod ->
            forM_ (findTypesToRefactor mod) \tr -> do
                putStrLn $ "Found datatype " <> GHC.printWithoutUniques tr.module_ <> "." <> GHC.printWithoutUniques tr.name <> " with fields " <> show (GHC.printWithoutUniques <$> tr.fieldNames)

data TypeToRefactor = TypeToRefactor
    { declaration :: GHC.TyClDecl GHC.GhcPs
    , module_ :: GHC.ModuleName
    , name :: GHC.RdrName
    , fieldNames :: [GHC.RdrName]
    }

findTypesToRefactor :: GHC.ParsedModule -> [TypeToRefactor]
findTypesToRefactor GHC.ParsedModule{GHC.pm_parsed_source=(unLoc -> mod)} =
    flip mapMaybe (GHC.hsmodDecls mod) \decl ->
        case unLoc decl of
            GHC.TyClD _ decl@(GHC.DataDecl{ GHC.tcdLName = unLoc -> nm, GHC.tcdDataDefn = GHC.HsDataDefn { GHC.dd_cons = GHC.DataTypeCons _ constructors } }) -> do
                let fieldNames = concatMap (getFieldNames . unLoc) constructors
                    getFieldNames GHC.ConDeclH98 { GHC.con_args = GHC.RecCon (unLoc -> fields) } =
                        concatMap (map (GHC.unLoc . GHC.foLabel . unLoc) . GHC.cd_fld_names . unLoc) fields
                    getFieldNames _ = [] -- GADTs not supported
                let hasLensPrefix fieldName =
                        case GHC.occNameString (GHC.rdrNameOcc fieldName) of
                            '_' : _ -> True
                            _ -> False
                guard (not $ null fieldNames)
                guard (all hasLensPrefix fieldNames)
                pure TypeToRefactor
                    { declaration = decl
                    , module_ = unLoc $ fromMaybe (error "c'mon, module with no name?") $ GHC.hsmodName mod
                    , name = nm
                    , fieldNames
                    }
            _ -> Nothing


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

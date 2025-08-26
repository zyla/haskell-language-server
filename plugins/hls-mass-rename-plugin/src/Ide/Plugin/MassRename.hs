{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

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
                                                           partition, split)
import           System.FilePath                          (takeExtension,
                                                           takeFileName)
import qualified Development.IDE.GHC.Compat as GHC
-- import Debug.Trace
import Control.Monad.Except (runExceptT)
import Data.Either (fromRight)
import Data.List (sort)
import Ide.Plugin.Error (getNormalizedFilePathE)
import Control.Monad.IO.Class (liftIO)
import Development.IDE.Core.PluginUtils (runActionE, useE)

-- import qualified Data.HashMap.Strict as HashMap
-- import Development.IDE.Core.OfInterest (setFilesOfInterest)

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

        -- Is this necessary?
        -- Without this we get warnings when typechecking
        -- But with this, HLS does a lot of stuff and slows down
        -- setFilesOfInterest ide $ HashMap.fromList $ map ((,OnDisk) . toNormalizedFilePath') absoluteFiles

        results <- runAction "GetModIface" ide $ uses GetModIface (map toNormalizedFilePath' absoluteFiles)
        let (succeeded, failed) = partition (isJust . fst) $ zip results absoluteFiles
        unless (null failed) $
            putStr $ unlines $ "Files that failed:" : map ((++) " * " . snd) failed

        fmap (fromRight (error "plugin error")) $ runExceptT $ forM_ succeeded $ \case
            (Just mod, fp) ->
                forM_ (findTypesToRefactor mod) \tr -> do
                    liftIO $ putStrLn $ "Found datatype " <> GHC.printWithoutUniques tr.module_ <> "." <> GHC.printWithoutUniques tr.name <> " with fields " <> show (GHC.printWithoutUniques <$> tr.fieldNames)
                    refs <- concat <$> mapM (Rename.refsAtName ide (toNormalizedFilePath' fp)) tr.fieldNames
                    forM_ (sort $ nubOrd refs) \loc -> do
                        nfp <- getNormalizedFilePathE loc._uri
                        (_, fileContents) <- runActionE "GetFileContents" ide $ useE GetFileContents nfp
                        liftIO $ putStrLn $ "  " <> show loc._uri
                        liftIO $ putStrLn $ "  " <> show fileContents
            _ -> pure ()

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
    case split (==':') $ GHC.occNameString $ GHC.nameOccName n of
        ["$sel", fieldName, _] -> fieldName
        xs -> error $ "unexpected field name: " <> show xs

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

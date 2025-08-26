{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

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
import           Development.IDE.Core.OfInterest          (setFilesOfInterest)
import qualified Data.HashMap.Strict                      as HashMap
import qualified System.Directory.Extra                   as IO
import           Control.Monad.Extra                      (concatMapM)
import           Data.List.Extra                          (isPrefixOf, nubOrd,
                                                           partition)
import           System.FilePath                          (takeExtension,
                                                           takeFileName)

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

            putStrLn "\nStep 4/4: Type checking the files"
            setFilesOfInterest ide $ HashMap.fromList $ map ((,OnDisk) . toNormalizedFilePath') absoluteFiles
            results <- runAction "User TypeCheck" ide $ uses TypeCheck (map toNormalizedFilePath' absoluteFiles)
            _results <- runAction "GetHie" ide $ uses GetHieAst (map toNormalizedFilePath' absoluteFiles)
            _results <- runAction "GenerateCore" ide $ uses GenerateCore (map toNormalizedFilePath' absoluteFiles)
            let (worked, failed) = partition fst $ zip (map isJust results) absoluteFiles
            when (failed /= []) $
                putStr $ unlines $ "Files that failed:" : map ((++) " * " . snd) failed

            let nfiles xs = let n' = length xs in if n' == 1 then "1 file" else show n' ++ " files"
            putStrLn $ "\nCompleted (" ++ nfiles worked ++ " worked, " ++ nfiles failed ++ " failed)"

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

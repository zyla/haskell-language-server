{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, unless)
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, listDirectory, getCurrentDirectory, setCurrentDirectory, copyPermissions)
import System.Environment (lookupEnv, setEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, assertFailure, assertEqual)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "MassRename CLI Tests"
    [ testCase "Integration: mass-rename transforms files correctly" testMassRenameIntegration
    ]

-- | Copy a directory recursively
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src dst = do
    createDirectoryIfMissing True dst
    items <- listDirectory src
    forM_ items $ \item -> do
        let srcPath = src </> item
            dstPath = dst </> item
        isDir <- doesDirectoryExist srcPath
        if isDir
            then copyDirectory srcPath dstPath
            else do
                copyFile srcPath dstPath
                copyPermissions srcPath dstPath

-- | Integration test that runs mass-rename and verifies output
testMassRenameIntegration :: IO ()
testMassRenameIntegration = withSystemTempDirectory "mass-rename-test" $ \tmpDir -> do
    let testDataDir = "plugins/hls-mass-rename-plugin/test/testdata/basic"
        expectedDir = testDataDir </> "expected"

    -- Get HLS executable path (build-tool-depends ensures it's in PATH)
    hlsExe <- fromMaybe "haskell-language-server" <$> lookupEnv "HLS_TEST_EXE"

    -- Copy test project to temp directory
    copyDirectory testDataDir tmpDir

    -- Save current directory and change to temp
    origDir <- getCurrentDirectory
    setCurrentDirectory tmpDir

    -- Build the test project to generate .hie files (only compiling files work)
    -- This will fail for UseWithoutConstructor but that's expected
    _ <- readProcessWithExitCode "cabal" ["build", "--ghc-options=-fwrite-ide-info"] ""

    -- Set APPLY=1 to actually modify files
    setEnv "APPLY" "1"

    -- Run mass-rename (binary is in PATH thanks to build-tool-depends)
    (exitCode, stdout, stderr) <- readProcessWithExitCode hlsExe ["mass-rename", "src"] ""

    -- Restore directory
    setCurrentDirectory origDir

    -- Check exit code
    case exitCode of
        ExitSuccess -> pure ()
        ExitFailure code -> assertFailure $
            "mass-rename failed with exit code " ++ show code ++
            "\nStdout: " ++ stdout ++
            "\nStderr: " ++ stderr

    -- Compare output files with expected (only files that compile)
    let filesToCheck =
            [ "Types1.hs"
            , "Types2.hs"
            , "Use.hs"
            , "UseSelector.hs"
--            , "UseWithConstructor.hs"
--            , "UseWithOpenImport.hs"
            -- Note: UseWithoutConstructor.hs won't be transformed because it doesn't compile
            -- (no .hie file generated), so we skip it
            ]

    forM_ filesToCheck $ \file -> do
        let actualPath = tmpDir </> "src" </> file
            expectedPath = expectedDir </> file

        actual <- T.readFile actualPath
        expected <- T.readFile expectedPath
        assertEqual ("File " ++ file ++ " should match expected output") expected actual

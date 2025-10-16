{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, forM_, unless)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, listDirectory, getCurrentDirectory, setCurrentDirectory, copyPermissions)
import System.Environment (lookupEnv, setEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, assertFailure)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "MassRename CLI Tests"
    [ testCase "Input files compile correctly" testInputFilesCompile
    , testCase "Integration: mass-rename transforms files correctly" testMassRenameIntegration
    ]

-- | Test that input files compile successfully
testInputFilesCompile :: IO ()
testInputFilesCompile = withSystemTempDirectory "mass-rename-compile-test" $ \tmpDir -> do
    let testDataDir = "plugins/hls-mass-rename-plugin/test/testdata/basic"

    -- Copy test project to temp directory
    copyDirectory testDataDir tmpDir

    -- Save current directory and change to temp
    origDir <- getCurrentDirectory
    setCurrentDirectory tmpDir

    -- Build the test project (should succeed for all exposed modules)
    (exitCode, stdout, stderr) <- readProcessWithExitCode "cabal" ["build"] ""

    -- Restore directory
    setCurrentDirectory origDir

    -- Check that build succeeded
    case exitCode of
        ExitSuccess -> pure ()
        ExitFailure code -> assertFailure $
            "Input files failed to compile (exit code " ++ show code ++ ")\n" ++
            "This indicates test data is broken.\n" ++
            "Stdout: " ++ stdout ++ "\n" ++
            "Stderr: " ++ stderr

-- | Copy a directory recursively, skipping certain directories
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src dst = do
    createDirectoryIfMissing True dst
    items <- listDirectory src
    forM_ items $ \item -> do
        -- Skip expected directory and build artifacts
        let skipItems = ["expected", "dist-newstyle", "hie.yaml"]
            skipPrefixes = [".ghc.environment"]
            shouldSkip = item `elem` skipItems || any (`isPrefixOf` item) skipPrefixes
        unless shouldSkip $ do
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

    -- Build the test project to generate .hie files
    _ <- readProcessWithExitCode "cabal" ["build", "--ghc-options=-fwrite-ide-info"] ""

    -- Set APPLY=1 to actually modify files
    setEnv "APPLY" "1"

    -- Run mass-rename (binary is in PATH thanks to build-tool-depends)
    (exitCode, stdout, stderr) <- readProcessWithExitCode hlsExe ["mass-rename", "--scan", "src", "--rewrite", "src"] ""

    -- Restore directory
    setCurrentDirectory origDir

    -- Print stderr for debugging
    putStrLn "=== mass-rename stderr ==="
    putStrLn stderr
    putStrLn "==========================="

    -- Check exit code
    case exitCode of
        ExitSuccess -> pure ()
        ExitFailure code -> assertFailure $
            "mass-rename failed with exit code " ++ show code ++
            "\nStdout: " ++ stdout ++
            "\nStderr: " ++ stderr

    -- Check if we should accept golden files (update expected outputs)
    acceptGolden <- lookupEnv "ACCEPT"

    -- Discover all .hs files in src directory
    srcFiles <- listDirectory (tmpDir </> "src")
    let filesToCheck = filter (".hs" `isSuffixOf`) srcFiles

    -- Collect all failures instead of stopping at the first one
    failures <- fmap concat $ forM filesToCheck $ \file -> do
        let actualPath = tmpDir </> "src" </> file
            expectedPath = expectedDir </> file

        -- Use diff to compare files - shows only differences
        (exitCode, diffOutput, _) <- readProcessWithExitCode "diff" ["-u", expectedPath, actualPath] ""

        case exitCode of
            ExitSuccess -> pure []  -- Files match
            _ -> case acceptGolden of
                Just _ -> do
                    -- Accept mode: update expected file with actual output
                    copyFile actualPath expectedPath
                    putStrLn $ "✓ Accepted golden file: " ++ file
                    pure []
                Nothing ->
                    -- Normal mode: collect diff for reporting
                    pure ["File " ++ file ++ " differs from expected:\n" ++ diffOutput]

    -- Report all failures at once
    unless (null failures) $
        assertFailure $ unlines failures

    -- Verify transformed files compile
    setCurrentDirectory tmpDir

    -- Clean build artifacts to force recompilation of transformed files
    _ <- readProcessWithExitCode "rm" ["-rf", "dist-newstyle"] ""

    -- Build the transformed project
    (buildExitCode, buildStdout, buildStderr) <- readProcessWithExitCode "cabal" ["build"] ""

    -- Restore directory
    setCurrentDirectory origDir

    -- Check that build succeeded
    case buildExitCode of
        ExitSuccess -> pure ()
        ExitFailure code -> assertFailure $
            "Transformed files failed to compile (exit code " ++ show code ++ ")\n" ++
            "This indicates the transformation produced invalid Haskell code.\n" ++
            "Stdout: " ++ buildStdout ++ "\n" ++
            "Stderr: " ++ buildStderr

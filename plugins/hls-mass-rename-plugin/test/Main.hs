{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, unless)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, assertFailure)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "MassRename CLI Tests"
    [ testCase "Test data files exist" testDataFilesExist
    , testCase "Expected output files exist" testExpectedFilesExist
    ]

-- | Verify that test input files exist
testDataFilesExist :: IO ()
testDataFilesExist = do
    let testDataDir = "plugins/hls-mass-rename-plugin/test/testdata/basic/src"
    exists <- doesDirectoryExist testDataDir
    unless exists $ assertFailure $ "Test data directory does not exist: " ++ testDataDir

    files <- listDirectory testDataDir
    let hsFiles = filter (\f -> takeExtension f == ".hs") files
    unless (length hsFiles >= 4) $
        assertFailure $ "Expected at least 4 .hs files in test data, found: " ++ show (length hsFiles)

-- | Verify that expected output files exist
testExpectedFilesExist :: IO ()
testExpectedFilesExist = do
    let expectedDir = "plugins/hls-mass-rename-plugin/test/testdata/basic/expected"
    exists <- doesDirectoryExist expectedDir
    unless exists $ assertFailure $ "Expected output directory does not exist: " ++ expectedDir

    files <- listDirectory expectedDir
    let hsFiles = filter (\f -> takeExtension f == ".hs") files
    unless (length hsFiles >= 4) $
        assertFailure $ "Expected at least 4 .hs files in expected output, found: " ++ show (length hsFiles)

{- TODO: Full integration test
   This test requires setting up a full IdeState which is complex.
   For now, the MassRename command can be tested manually using:

   $ cd plugins/hls-mass-rename-plugin/test/testdata/basic
   $ APPLY=1 haskell-language-server-wrapper mass-rename src

   Then compare the output in src/ with the expected/ directory.
-}

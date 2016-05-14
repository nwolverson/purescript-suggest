module Test.Main where

import Prelude
import Data.List as List
import Test.Unit.Assert as Assert
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (isLeft, Either(Right))
import Suggest (replaceFile')
import Test.Unit (test, suite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall e. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT | e) Unit
main = runTest do
  suite "suggestions" do
    test "replace multi-line" do
      let replacements = replace (List.singleton (testReplacement 2 3 "REPLACEMENT"))
      Assert.equal (Right $ List.fromFoldable [ "Line 1", "REPLACEMENT", "Line 4", "Line 5" ]) replacements
    test "replace single-line" do
      let replacements = replace (List.singleton (testReplacement 2 2 "REPLACEMENT"))
      Assert.equal (Right $ List.fromFoldable [ "Line 1", "REPLACEMENT", "Line 3", "Line 4", "Line 5" ]) replacements
    test "2 replacements with gap" do
      let replacements = replace (List.fromFoldable [testReplacement 2 2 "TEXT1", testReplacement 4 4 "TEXT2"])
      Assert.equal (Right $ List.fromFoldable [ "Line 1", "TEXT1", "Line 3", "TEXT2", "Line 5" ]) replacements
    test "2 replacements with no gap" do
      let replacements = replace (List.fromFoldable [testReplacement 2 3 "TEXT1", testReplacement 4 4 "TEXT2"])
      Assert.equal (Right $ List.fromFoldable [ "Line 1", "TEXT1", "TEXT2", "Line 5" ]) replacements
    test "replacement after end of file" do
      let replacements = replace (List.fromFoldable [testReplacement 10 20 "ERROR"])
      Assert.assert "should be Left" $ isLeft replacements
    test "replacements overlap" do
      let replacements = replace (List.fromFoldable [testReplacement 2 4 "TEXT1", testReplacement 3 5 "TEXT2"])
      Assert.assert "should be Left" $ isLeft replacements

  where
  -- psc line indexing is 1-based
  replace = replaceFile' 1 (List.fromFoldable testFile)
  testFile = [
      "Line 1"
    , "Line 2"
    , "Line 3"
    , "Line 4"
    , "Line 5"
  ]
  testFileName = "/path/to/my/file.purs"

  testReplacement startLine endLine replacement =
    { filename: testFileName
    , position: { startLine, startColumn: 0, endLine, endColumn: 0 }
    , original: "I think this is unused"
    , replacement
    }

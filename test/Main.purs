module Test.Main where

import Prelude

import Data.Either (isLeft, Either(Right))
import Data.Foldable (intercalate)
import Data.List as List
import Effect (Effect)
import Suggest (replaceFile')
import Test.Unit (test, suite)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

-- TODO: If suggestion starts at the start of a line, and ends at the end, consume the trailing \n

main :: Effect Unit
main = runTest do
  suite "suggestions" do
    test "replace multi-line" do
      let replacements = replace (List.singleton (testReplacement 2 3 "REPLACEMENT"))
      Assert.equal (result [ "Line 1", "REPLACEMENT", "Line 4", "Line 5" ]) replacements
    test "replace single-line" do
      let replacements = replace (List.singleton (testReplacement 2 2 "REPLACEMENT"))
      Assert.equal (result [ "Line 1", "REPLACEMENT", "Line 3", "Line 4", "Line 5" ]) replacements
    test "2 replacements with gap" do
      let replacements = replace (List.fromFoldable [testReplacement 2 2 "TEXT1", testReplacement 4 4 "TEXT2"])
      Assert.equal (result [ "Line 1", "TEXT1", "Line 3", "TEXT2", "Line 5" ]) replacements
    test "2 replacements with no gap" do
      let replacements = replace (List.fromFoldable [testReplacement 2 3 "TEXT1", testReplacement 4 4 "TEXT2"])
      Assert.equal (result [ "Line 1", "TEXT1", "TEXT2", "Line 5" ]) replacements
    test "replacement after end of file" do
      let replacements = replace (List.fromFoldable [testReplacement 10 20 "ERROR"])
      Assert.assert "should be Left" $ isLeft replacements
    test "replacements overlap" do
      let replacements = replace (List.fromFoldable [testReplacement 2 4 "TEXT1", testReplacement 3 5 "TEXT2"])
      Assert.assert "should be Left" $ isLeft replacements
  suite "removing lines" do
    test "remove single line" do
      let replacements = replace (List.singleton (testReplacement 2 2 ""))
      Assert.equal (result [ "Line 1", "Line 3", "Line 4", "Line 5" ]) replacements
    test "remove two lines" do
      let replacements = replace (List.singleton (testReplacement 2 3 ""))
      Assert.equal (result [ "Line 1", "Line 4", "Line 5" ]) replacements
    test "remove two separate lines" do
      let replacements = replace (List.fromFoldable [testReplacement 2 2 "", testReplacement 4 4 ""])
      Assert.equal (result [ "Line 1", "Line 3", "Line 5" ]) replacements
  suite "suggestions within a line" do
    test "suggestion within single line" do
      let replacements = replace (List.singleton (testReplacement' 2 2 2 4 "_"))
      Assert.equal (result [ "Line 1", "L_e 2", "Line 3", "Line 4", "Line 5" ]) replacements
    test "suggestions across multiple lines" do
      let replacements = replace (List.singleton (testReplacement' 2 2 3 4 "_"))
      Assert.equal (result [ "Line 1", "L_e 3", "Line 4", "Line 5" ]) replacements
    test "multiple suggestions on one line" do
      let replacements = replace (List.fromFoldable [testReplacement' 2 1 2 2 "_", testReplacement' 2 4 2 6 "_"])
      Assert.equal (result [ "Line 1", "_in_2", "Line 3", "Line 4", "Line 5" ]) replacements
    test "multi-line replacement" do
      let replacements = replace (List.fromFoldable [testReplacement' 2 4 2 6 "_\n_\n_"])
      Assert.equal (result
        [ "Line 1"
        , "Lin_"
        , "   _"
        , "   _2"
        , "Line 3"
        , "Line 4"
        , "Line 5"
        ]) replacements
    test "multi-line replacement (start of line)" do
      let replacements = replace (List.fromFoldable [testReplacement' 2 1 2 2 "_\n_\n_"])
      Assert.equal (result
        [ "Line 1"
        , "_"
        , "_"
        , "_ine 2"
        , "Line 3"
        , "Line 4"
        , "Line 5"
        ]) replacements
    test "multi-line replacement trims trailing whitespace before newlines" do
      let replacements = replace (List.fromFoldable [testReplacement' 2 4 2 6 "_ \n_   \n_  "])
      Assert.equal (result
        [ "Line 1"
        , "Lin_"
        , "   _"
        , "   _2"
        , "Line 3"
        , "Line 4"
        , "Line 5"
        ]) replacements

  where
  -- psc line indexing is 1-based
  replace x = intercalate "" <$> replaceFile' 1 1 (List.fromFoldable testFile) x
  result = Right <<< intercalate "\n"
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
    , position: { startLine, startColumn: 1, endLine, endColumn: 7 }
    , original: "I think this is unused"
    , replacement
    }

  testReplacement' startLine startColumn endLine endColumn replacement =
    { filename: testFileName
    , position: { startLine, startColumn, endLine, endColumn }
    , original: "I think this is unused"
    , replacement
    }

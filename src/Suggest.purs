module Suggest where

import Prelude

import Data.Array (replicate, mapMaybe, concat, head, groupBy, catMaybes, sortBy)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(Right, Left), either)
import Data.Foldable (for_, intercalate)
import Data.List (List(Nil, Cons), drop, (!!), length, fromFoldable)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.String (trim, joinWith)
import Data.String as Str
import Data.String.Regex (regex, test, replace) as Regex
import Data.String.Regex.Flags (noFlags, global) as Regex
import Data.Traversable (fold, traverse)
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.Encoding as Encoding
import Node.FS.Sync as File
import Psa (PsaError, Position, PsaPath(Src), compareByLocation, annotatedError)

type Replacement =
  { filename :: String
  , position :: Position
  , original :: String
  , replacement :: String
}

type Suggestions = { replacements:: Array (Array Replacement), files:: Ref (Map String (Array String)) }

getSuggestions :: Array PsaError -> Effect Suggestions
getSuggestions warnings = do
  files <- Ref.new Map.empty
  let loadLinesImpl = loadLines files
  warnings' <- sortBy compareByLocation <$> catMaybes <$> traverse (annotateError loadLinesImpl) warnings
  let replacements = mapMaybe getReplacement warnings'
  pure { replacements: map NEA.toArray (groupBy (\a b -> a.filename == b.filename) replacements), files }
  where
    loadLines files filename pos = do
      contents <- Ref.read files >>= \cache ->
        case Map.lookup filename cache of
          Just lines -> pure lines
          Nothing -> do
            lines <- Str.split (Str.Pattern "\n") <$> File.readTextFile Encoding.UTF8 filename
            Ref.modify_ (Map.insert filename lines) files
            pure lines
      let source = Array.slice (pos.startLine - 1) (pos.endLine) contents
      pure $ Just source

    getReplacement { source: Just s, error: { suggestion: Just { replacement, replaceRange }, filename: Just filename }, position: Just position }
     =
      Just {
        filename,
        position: fromMaybe position replaceRange,
        original: joinWith "\n" s,
        replacement
      }
    getReplacement _ = Nothing

    annotateError loadLinesF error = do
      source <- fromMaybe (pure Nothing) (loadLinesF <$> error.filename <*> error.position)
      pure $ annotatedError <$> (Src <$> error.filename) <*> pure source <*> pure error

applySuggestions :: Array PsaError -> Effect Unit
applySuggestions warnings = do
  { replacements, files } <- getSuggestions warnings
  for_ replacements $ \group ->
    case head group of
      Just { filename } -> do
        log $ "Processing " <> filename
        Ref.read files >>= \res -> case Map.lookup filename res of
          Just lines -> replaceFile lines filename group
          _ -> pure unit
      Nothing -> pure unit

listSuggestions :: Array PsaError -> Effect String
listSuggestions warnings = do
  { replacements, files } <- getSuggestions warnings
  let totalCount = Array.length (concat replacements)
      msg = "Found " <> show totalCount <> " suggestions in " <> show (Array.length replacements) <> " files.\n"
      msg' = intercalate "\n" $ mapMaybe rep replacements
  pure $ msg <> msg'
  where
  rep reps = case head reps of
    Just { filename } -> Just $ filename <> ": " <> show (Array.length reps) <> " replacements"
    _ -> Nothing

replaceFile :: Array String -> String -> Array Replacement -> Effect Unit
replaceFile lines filename group =
  case replaceFile' 1 1 (fromFoldable lines) (fromFoldable group) of
    Left err -> error err
    Right outLines -> do
      File.writeTextFile Encoding.UTF8 filename $ intercalate "" outLines
      log $ filename <> ": Applied " <> show (Array.length group) <> " fixes"

-- | This is where all the real work happens.
-- | Steps through the source file, outputting replacement text when the position
-- | matches otherwise the original text. Objects if replacements overlap or go past the file end.
replaceFile' :: Int -> Int -> List String -> List Replacement -> Either String (List String)
replaceFile' n _ lines reps@(Cons { position: { startLine } } _) | n < startLine && length lines >= startLine - n =
  (withNewlines (List.take count lines) <> _) <$> replaceFile' startLine 1 (drop count lines) reps
  where
    count = startLine - n
replaceFile' n m lines (Cons r@{ position: { startLine, startColumn, endLine, endColumn }, original, replacement } reps) | n == startLine =
  let initial = Str.take (startColumn - m) (fromMaybe "" $ List.head lines)
      final = Str.drop (endColumn - (if startLine == endLine then m else 1)) (fromMaybe "" $ lines !! (endLine - startLine))
      trailingNewline = either (const true) (\regex -> Regex.test regex replacement) (Regex.regex "\n\\s+$" Regex.noFlags)
      addNewline = trailingNewline && (not $ Str.null final)
      replace regex s text = either (const text) (\regex' -> Regex.replace regex' s text) (Regex.regex regex Regex.global)
      tweak = replace "\\n(.)" ("\n" <> fold (replicate (startColumn-1) " ") <> "$1") >>>
              replace "\\s+\\n" "\n" >>>
              trim
      newText = initial <> tweak replacement <> (if addNewline then "\n" else "")
      replaceNewText = case newText of
        "" -> identity
        _ -> Cons newText
      remainingLines = (drop (endLine - startLine + 1) lines)
  in
    if final == "" && newText == "" then
      -- Avoid blank lines when replacing entire line(s) with blank
      replaceNewText <$> replaceFile' (endLine+1) 1 remainingLines reps
    else
      replaceNewText <$> replaceFile' endLine endColumn (Cons final remainingLines) reps

replaceFile' n _ _ reps@(Cons { position: { startLine } } _) | n > startLine =
  Left $ "Found replacement starting before current position: " <> show startLine <> ", " <> show n
replaceFile' _ _ lines Nil = pure $ intercalate (Cons "\n" Nil) (List.singleton <$> lines)
replaceFile' _ _ _ _ = Left "Found replacement after end of file"

withNewlines :: List String -> List String
withNewlines = List.concatMap (\x -> Cons x (Cons "\n" Nil))

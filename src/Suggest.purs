module Suggest where

import Prelude
import Data.Array as Array
import Data.List as List
import Data.StrMap.ST as STMap
import Data.String as Str
import Node.Encoding as Encoding
import Node.FS.Sync as File
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.ST (ST)
import Data.Array (replicate, mapMaybe, concat, head, groupBy, catMaybes, sortBy)
import Data.Either (Either(Right, Left), either)
import Data.Foldable (for_, intercalate)
import Data.List (List(Nil, Cons), drop, (!!), length, fromFoldable)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.NonEmpty (fromNonEmpty)
import Data.StrMap.ST (STStrMap)
import Data.String (trim, joinWith)
import Data.String.Regex (regex, test, replace) as Regex
import Data.String.Regex.Flags (noFlags, global) as Regex
import Data.Traversable (fold, traverse)
import Node.FS (FS)
import Psa (PsaError, PsaAnnotedError, Position, PsaPath(Src), compareByLocation, annotatedError)

type Replacement =
  { filename :: String
  , position :: Position
  , original :: String
  , replacement :: String
}

type SEff eff h = (console :: CONSOLE, err :: EXCEPTION, fs :: FS, st :: ST h | eff)

-- This alias avoids psc bug which should be fixed in 9.0
type PsaAnnotedErrors = Array PsaAnnotedError

type Suggestions h = { replacements:: Array (Array Replacement), files:: STStrMap h (Array String)}

getSuggestions :: forall eff h. Array PsaError -> Eff (SEff eff h) (Suggestions h)
getSuggestions warnings = do
  files <- STMap.new
  let loadLinesImpl = loadLines files
  warnings' :: PsaAnnotedErrors <- sortBy compareByLocation <$> catMaybes <$> traverse (annotateError loadLinesImpl) warnings
  let replacements = mapMaybe getReplacement warnings'
  pure { replacements: fromNonEmpty Array.cons <$> groupBy (\a b -> a.filename == b.filename) replacements, files: files }
  where
    loadLines files filename pos = do
      contents <- STMap.peek files filename >>= \cache ->
        case cache of
          Just lines -> pure lines
          Nothing -> do
            lines <- Str.split (Str.Pattern "\n") <$> File.readTextFile Encoding.UTF8 filename
            STMap.poke files filename lines
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

applySuggestions :: forall eff h. Array PsaError -> Eff (SEff eff h) Unit
applySuggestions warnings = do
  { replacements, files } <- getSuggestions warnings
  for_ replacements $ \group ->
    case head group of
      Just { filename } -> do
        log $ "Processing " <> filename
        STMap.peek files filename >>= \res -> case res of
          Just lines -> replaceFile lines filename group
          _ -> pure unit
      Nothing -> pure unit

listSuggestions :: forall eff h. Array PsaError -> Eff (SEff eff h) String
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

replaceFile :: forall eff. Array String -> String -> Array Replacement -> Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS | eff) Unit
replaceFile lines filename group = do
  let group' = (fromFoldable group) :: List Replacement
  let lines' = (fromFoldable lines) :: List String
  let replaced = replaceFile' 1 1 lines' group'
  case replaced of
    Left err -> error err
    Right lines -> do
      File.writeTextFile Encoding.UTF8 filename $ intercalate "" lines
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
        "" -> id
        _ -> Cons newText
  in
    replaceNewText <$> replaceFile' endLine endColumn (Cons final (drop (endLine - startLine + 1) lines)) reps

replaceFile' n _ _ reps@(Cons { position: { startLine } } _) | n > startLine =
  Left $ "Found replacement starting before current position: " <> show startLine <> ", " <> show n
replaceFile' _ _ lines Nil = pure $ intercalate (Cons "\n" Nil) (List.singleton <$> lines)
replaceFile' _ _ _ _ = Left "Found replacement after end of file"

withNewlines :: List String -> List String
withNewlines = List.concatMap (\x -> Cons x (Cons "\n" Nil))

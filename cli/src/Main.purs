module Main where

import Prelude

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (length, drop)
import Data.Either (Either(Right))
import Data.Foldable (for_)
import Data.String (split, Pattern(..))
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Ref as Ref
import Node.Encoding (Encoding(UTF8))
import Node.Process (stdin, argv)
import Node.Stream (onEnd, onDataString)
import Psa (parsePsaResult)
import Suggest (listSuggestions, applySuggestions)

data Action = Apply | List | Help Boolean

parseArgs :: Effect Action
parseArgs = do
  argv <- drop 2 <$> argv
  pure $ case argv of
    [ "--apply" ] -> Apply
    [ "--list" ]  -> List
    []            -> List
    [ "--help" ]  -> Help false
    _             -> Help true

main :: Effect Unit
main = do
  action <- parseArgs
  case action of
    Help isUnrecognised -> do
      when isUnrecognised $ error "Unrecognised arguments"
      log "Usage: ps-suggest [--list | --apply]\nJSON compiler errors must be supplied on stdin. You probably want to pipe these from psa."
    _ -> do
      inputRef <- Ref.new ""
      onDataString stdin UTF8 $ \s -> do
        Ref.modify_ (_ <> s) inputRef
      onEnd stdin do
        input <- Ref.read inputRef
        foundSuggestions <- Ref.new false
        for_ (split (Pattern "\n") input) \line ->
          case jsonParser line >>= decodeJson >>= parsePsaResult of
            Right { warnings } | length warnings > 0 -> do
              Ref.write true foundSuggestions
              case action of
                List -> listSuggestions warnings >>= log
                Apply -> applySuggestions warnings
                _ -> pure unit
            _ -> pure unit
        Ref.read foundSuggestions >>= \found ->
          unless found $ log "No suggestions found."

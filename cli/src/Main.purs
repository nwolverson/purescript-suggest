module Main where

import Prelude
import Control.Monad (when, unless)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (error, CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (writeRef, readRef, modifyRef, newRef, REF)
import Control.Monad.ST (ST)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (length, drop)
import Data.Either (Either(Right))
import Data.Foldable (for_)
import Data.String (split)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.Process (PROCESS, stdin, argv)
import Node.Stream (onEnd, onDataString)
import Psa (parsePsaResult)
import Suggest (listSuggestions, applySuggestions)

data Action = Apply | List | Help Boolean

parseArgs :: forall eff. Eff (process :: PROCESS | eff) Action
parseArgs = do
  argv <- drop 2 <$> argv
  pure $ case argv of
    [ "--apply" ] -> Apply
    [ "--list" ]  -> List
    []            -> List
    [ "--help" ]  -> Help false
    _             -> Help true

main :: forall e h. Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS, st :: ST h, ref :: REF, process :: PROCESS | e) Unit
main = do
  action <- parseArgs
  case action of
    Help isUnrecognised -> do
      when isUnrecognised $ error "Unrecognised arguments"
      log "Usage: ps-suggest [--list | --apply]\nJSON compiler errors must be supplied on stdin. You probably want to pipe these from psa."
    _ -> do
      inputRef <- newRef ""
      onDataString stdin UTF8 $ \s -> do
        modifyRef inputRef (_ <> s)
      onEnd stdin do
        input <- readRef inputRef
        foundSuggestions <- newRef false
        for_ (split "\n" input) \line ->
          case jsonParser line >>= decodeJson >>= parsePsaResult of
            Right { warnings } | length warnings > 0 -> do
              writeRef foundSuggestions true
              case action of
                List -> listSuggestions warnings >>= log
                Apply -> applySuggestions warnings
                _ -> pure unit
            _ -> pure unit
        readRef foundSuggestions >>= \found ->
          unless found $ log "No suggestions found."

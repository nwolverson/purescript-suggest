{ name = "suggest"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-process"
  , "node-streams"
  , "ordered-collections"
  , "prelude"
  , "psa-utils"
  , "refs"
  , "strings"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "cli/**/*.purs", "test/**/*.purs" ]
}

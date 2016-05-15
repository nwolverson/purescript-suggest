# purescript-suggest

Command line tool (and library) to apply the suggested fixes from `psc` for warnings, such as
removing redundant imports, or making imports explicit.

# Warning

This is ALPHA software that modifies your source code in place by design. Errors in
this code, or any issues with the warnings passed in, could result in the deletion
of your precious source code.

COMMIT EARLY COMMIT OFTEN.

## Known issues

Not currently handling any error suggestions which don't span entire lines. As of
the compiler version 0.8.5 I think this is OK but by 0.9.0 improvements will be required.

# Installation

To install `ps-suggest` globally:
```
npm install -g purescript-suggest
```

## Usage
Pipe the JSON compiler output to stdin of `ps-suggest`. You probably want to use
`psa`, possibly with a warning stash and almost certainly filtering out library
errors. This also gives you the chance to choose which codes to replace.

To check changes that will be applied:
```
  pulp --stash --censor-lib --json-errors 2>&1 | ps-suggest --list
```

To apply the changes to all affected files:
```
  pulp --stash --censor-lib --json-errors 2>&1 | ps-suggest --apply
```

# Library usage

Install `purescript-suggest` via bower and use something like `Publish.applySuggestions`, which takes
already parsed errors ala `purescript-psa`.

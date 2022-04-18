#!/usr/bin/env bash
# shellcheck shell=bash

set -xeuo pipefail
IFS=$'\n\t'

git ls-tree -z -r HEAD --name-only | grep -z '\.cabal$' | xargs -0 cabal format
echo 'SUCCESS: Cabal files formatted'

git ls-tree -z -r HEAD --name-only | grep -z '\.nix$' | xargs -0 nixpkgs-fmt
echo 'SUCCESS: Nix files formatted'

git ls-tree -z -r HEAD --name-only | grep -z '\.\(yaml\|yml\|json\)$' | xargs -0 prettier --write
echo 'SUCCESS: YAML and JSON files formatted'

git ls-tree -z -r HEAD --name-only | grep -z '\.py$' | xargs -0 black
echo 'SUCCESS: Python files formatted'

function haskell_files {
    git ls-tree -z -r HEAD --name-only | grep -z '\.hs$'
}

haskell_files | xargs -0 -I{} hlint {} --refactor --with-refactor=refactor --refactor-options="--inplace"
echo 'SUCCESS: Applied HLint suggestions'

haskell_files | xargs -0 -I{} ormolu --mode inplace {}
echo 'SUCCESS: Formatted Haskell files with Ormolu'

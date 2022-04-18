#!/usr/bin/env bash
# shellcheck shell=bash

set -euo pipefail
IFS=$'\n\t'

fixmsg="Run 'eval bin/fix.sh)' to fix this"

if ! git diff --quiet; then
    echo 'ERROR: Dirty working directory'
    exit 1
fi

git ls-tree -z -r HEAD --name-only | grep -z '\.cabal$' | xargs -0 cabal format
if ! git diff --exit-code; then
    echo 'FAILURE: Cabal files were not formatted correctly'
    echo "$fixmsg"
    exit 1
fi
echo 'SUCCESS: Cabal files are formatted correctly'

git ls-tree -z -r HEAD --name-only | grep -z '\.nix$' | xargs -0 nixpkgs-fmt
if ! git diff --exit-code; then
    echo 'FAILURE: Nix files were not formatted correctly'
    echo "$fixmsg"
    exit 1
fi
echo 'SUCCESS: Nix files are formatted correctly'

if ! git ls-tree -z -r HEAD --name-only | grep -z '\.\(yaml\|yml\|json\)$' | xargs -0 prettier --list-different; then
    echo 'FAILURE: YAML and JSON files were not formatted correctly'
    echo "$fixmsg"
    exit 1
fi
echo 'SUCCESS: YAML and JSON files are formatted correctly'

if ! git ls-tree -z -r HEAD --name-only | grep -z '\.py$' | xargs -0 black --check --diff 2>/dev/null; then
    echo 'FAILURE: Python files were not formatted correctly'
    echo "$fixmsg"
    exit 1
fi
echo 'SUCCESS: Python files formatted'

if ! git ls-tree -z -r HEAD --name-only | grep -z '\.sh$' | xargs -0 shellcheck ; then
    echo 'FAILURE: Bash files failed checks'
    exit 1
fi
echo 'SUCCESS: Bash files passed checks'


function haskell_files {
    git ls-tree -z -r HEAD --name-only | grep -z '\.hs$'
}

if ! haskell_files | xargs -0 hlint; then
    echo 'FAILURE: HLint reported issues'
    echo "$fixmsg"
    echo 'Manual fixes may be necessary'
    exit 1
fi
echo 'SUCCESS: HLint reported no issues'

if ! haskell_files | xargs -0 -I{} ormolu --mode check {}; then
    echo 'FAILURE: Haskell files are not formatted correctly'
    echo "$fixmsg"
    exit 1
fi
echo 'SUCCESS: Haskell files are formatted correctly'

{ cabal-install
, git
, hlint
, nixpkgs-fmt
, nodePackages
, ormolu
, python37Packages
, shellcheck
, writeScript
}:

writeScript "lint.sh" ''
  #!/usr/bin/env bash
  # shellcheck shell=bash

  set -euo pipefail
  IFS=$'\n\t'

  black="${python37Packages.black}/bin/black"
  cabal="${cabal-install}/bin/cabal"
  git="${git}/bin/git"
  hlint="${hlint}/bin/hlint"
  nixpkgsfmt="${nixpkgs-fmt}/bin/nixpkgs-fmt"
  ormolu="${ormolu}/bin/ormolu"
  prettier="${nodePackages.prettier}/bin/prettier"
  shellcheck="${shellcheck}/bin/shellcheck"

  fixmsg="Run 'eval \$(nix-build . -A fix)' to fix this"

  if ! $git diff --quiet; then
      echo 'ERROR: Dirty working directory'
      exit 1
  fi

  $git ls-tree -z -r HEAD --name-only | grep -z '\.cabal$' | xargs -0 $cabal format
  if ! $git diff --exit-code; then
      echo 'FAILURE: Cabal files were not formatted correctly'
      echo "$fixmsg"
      exit 1
  fi
  echo 'SUCCESS: Cabal files are formatted correctly'

  $git ls-tree -z -r HEAD --name-only | grep -z '\.nix$' | xargs -0 $nixpkgsfmt
  if ! $git diff --exit-code; then
      echo 'FAILURE: Nix files were not formatted correctly'
      echo "$fixmsg"
      exit 1
  fi
  echo 'SUCCESS: Nix files are formatted correctly'

  if ! $git ls-tree -z -r HEAD --name-only | grep -z '\.\(yaml\|yml\|json\)$' | xargs -0 $prettier --list-different; then
      echo 'FAILURE: YAML and JSON files were not formatted correctly'
      echo "$fixmsg"
      exit 1
  fi
  echo 'SUCCESS: YAML and JSON files are formatted correctly'

  if ! $git ls-tree -z -r HEAD --name-only | grep -z '\.py$' | xargs -0 $black --check --diff 2>/dev/null; then
      echo 'FAILURE: Python files were not formatted correctly'
      echo "$fixmsg"
      exit 1
  fi
  echo 'SUCCESS: Python files formatted'

  # List all generated Bash scripts here.
  bash_files=(
    "$(nix-build . -A lint)"
    "$(nix-build . -A fix)"
    "$(nix-build . -A update)"
  )
  if ! $shellcheck "$($git ls-tree -r HEAD --name-only | grep '\.sh')" "''${bash_files[@]}"; then
      echo 'FAILURE: Bash files failed checks'
      exit 1
  fi
  echo 'SUCCESS: Bash files passed checks'


  function haskell_files {
      $git ls-tree -z -r HEAD --name-only | grep -z '\.hs$'
  }

  if ! haskell_files | xargs -0 $hlint; then
      echo 'FAILURE: HLint reported issues'
      echo "$fixmsg"
      echo 'Manual fixes may be necessary'
      exit 1
  fi
  echo 'SUCCESS: HLint reported no issues'

  if ! haskell_files | xargs -0 -I{} $ormolu --mode check {}; then
      echo 'FAILURE: Haskell files are not formatted correctly'
      echo "$fixmsg"
      exit 1
  fi
  echo 'SUCCESS: Haskell files are formatted correctly'
''

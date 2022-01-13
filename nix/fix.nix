{ refactor
, cabal-install
, git
, hlint
, nixpkgs-fmt
, nodePackages
, ormolu
, pkgs
, python37Packages
, writeScript
}:

writeScript "fix.sh" ''
  #!/usr/bin/env ${pkgs.bash}/bin/bash
  # shellcheck shell=bash

  set -xeuo pipefail
  IFS=$'\n\t'

  black="${python37Packages.black}/bin/black"
  cabal="${cabal-install}/bin/cabal"
  git="${git}/bin/git"
  hlint="${hlint}/bin/hlint"
  nixpkgsfmt="${nixpkgs-fmt}/bin/nixpkgs-fmt"
  ormolu="${ormolu}/bin/ormolu"
  prettier="${nodePackages.prettier}/bin/prettier"
  refactor="${refactor}/bin/refactor"

  $git ls-tree -z -r HEAD --name-only | grep -z '\.cabal$' | xargs -0 $cabal format
  echo 'SUCCESS: Cabal files formatted'

  $git ls-tree -z -r HEAD --name-only | grep -z '\.nix$' | xargs -0 $nixpkgsfmt
  echo 'SUCCESS: Nix files formatted'

  $git ls-tree -z -r HEAD --name-only | grep -z '\.\(yaml\|yml\|json\)$' | xargs -0 $prettier --write
  echo 'SUCCESS: YAML and JSON files formatted'

  $git ls-tree -z -r HEAD --name-only | grep -z '\.py$' | xargs -0 $black
  echo 'SUCCESS: Python files formatted'

  function haskell_files {
      $git ls-tree -z -r HEAD --name-only | grep -z '\.hs$'
  }

  haskell_files | xargs -0 -I{} $hlint {} --refactor --with-refactor=$refactor --refactor-options="--inplace"
  echo 'SUCCESS: Applied HLint suggestions'

  haskell_files | xargs -0 -I{} $ormolu --mode inplace {}
  echo 'SUCCESS: Formatted Haskell files with Ormolu'
''

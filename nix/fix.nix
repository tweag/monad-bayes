{ apply-refact
, cabal-install
, hlint
, pkgs
, writeScript
}:

writeScript "fix.sh" ''
  #!/usr/bin/env ${pkgs.bash}/bin/bash

  set -euo pipefail
  IFS=$'\n\t'

  cabal="${cabal-install}/bin/cabal"
  git="${pkgs.git}/bin/git"
  hlint="${hlint}/bin/hlint"
  nixpkgsfmt="${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt"
  ormolu="${pkgs.ormolu}/bin/ormolu"
  prettier="${pkgs.nodePackages.prettier}/bin/prettier"
  refactor="${apply-refact}/bin/refactor"

  $git ls-tree -z -r HEAD --name-only | grep -z '\.cabal$' | xargs -0 $cabal format
  echo 'SUCCESS: Cabal files formatted'

  $git ls-tree -z -r HEAD --name-only | grep -z '\.nix$' | xargs -0 $nixpkgsfmt
  echo 'SUCCESS: Nix files formatted'

  $git ls-tree -z -r HEAD --name-only | grep -z '\.\(yaml\|yml\|json\)$' | xargs -0 $prettier --write
  echo 'SUCCESS: YAML and JSON files formatted'

  function haskell_files {
      $git ls-tree -z -r HEAD --name-only | grep -z '\.hs$'
  }

  haskell_files | xargs -0 -I{} $hlint {} --refactor --with-refactor=$refactor --refactor-options="--inplace"
  echo 'SUCCESS: Applied HLint suggestions'

  haskell_files | xargs -0 -I{} $ormolu --mode inplace {}
  echo 'SUCCESS: Formatted Haskell files with Ormolu'
''

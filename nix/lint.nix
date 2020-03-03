{ cabal-install
, hlint
, pkgs
, writeScript
}:

writeScript "lint.sh" ''
  set -euo pipefail
  IFS=$'\n\t'

  cabal="${cabal-install}/bin/cabal"
  git="${pkgs.git}/bin/git"
  hlint="${hlint}/bin/hlint"
  nixpkgsfmt="${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt"
  ormolu="${pkgs.ormolu}/bin/ormolu"

  if ! $git diff --quiet; then
      echo 'ERROR: Dirty working directory'
      exit 1
  fi

  $git ls-tree -z -r HEAD --name-only | grep -z '\.cabal$' | xargs -0 $cabal format
  if ! $git diff --exit-code; then
      echo 'FAILURE: Cabal files were not formatted correctly'
      echo 'Run "eval $(nix-build -A fix)" to fix this'
      exit 1
  fi
  echo 'SUCCESS: Cabal files are formatted correctly'

  $git ls-tree -z -r HEAD --name-only | grep -z '\.nix$' | xargs -0 $nixpkgsfmt
  if ! $git diff --exit-code; then
      echo 'FAILURE: Nix files were not formatted correctly'
      echo 'Run "eval $(nix-build -A fix)" to fix this'
      exit 1
  fi
  echo 'SUCCESS: Nix files are formatted correctly'

  function haskell_files {
      $git ls-tree -z -r HEAD --name-only | grep -z '\.hs$'
  }

  if ! haskell_files | xargs -0 $hlint; then
      echo 'FAILURE: HLint reported issues'
      echo 'Run "eval $(nix-build -A fix)" to attempt to fix this, manual fixes may be necessary'
      exit 1
  fi
  echo 'SUCCESS: HLint reported no issues'

  if ! haskell_files | xargs -0 -I{} $ormolu --mode check {}; then
      echo 'FAILURE: Haskell files are not formatted correctly'
      echo 'Run "eval $(nix-build -A fix)" to fix this'
      exit 1
  fi
  echo 'SUCCESS: Haskell files are formatted correctly'
''

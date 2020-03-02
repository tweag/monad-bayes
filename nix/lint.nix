{ cabal-install
, pkgs
, writeScript
}:

writeScript "lint.sh" ''
  set -euo pipefail
  IFS=$'\n\t'

  git="${pkgs.git}/bin/git"
  cabal="${cabal-install}/bin/cabal"

  if ! $git diff --quiet -- *.cabal; then
      echo 'ERROR: Dirty working directory'
      exit 1
  fi
  $cabal format *.cabal
  if ! $git diff --quiet -- *.cabal; then
      echo 'FAILURE: Cabal files were not formatted correctly'
      exit 1
  fi

  echo 'SUCCESS: Cabal files are formatted correctly'
''

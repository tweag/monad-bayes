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

  if ! $git diff --quiet -- *.cabal; then
      echo 'ERROR: Dirty working directory'
      exit 1
  fi

  $cabal format *.cabal
  if ! $git diff --exit-code -- *.cabal; then
      echo 'FAILURE: Cabal files were not formatted correctly'
      echo 'Run "eval $(nix-build -A fix)" to fix this'
      exit 1
  fi
  echo 'SUCCESS: Cabal files are formatted correctly'

  if ! $git ls-tree -z -r HEAD --name-only | grep -z '\.hs$' | xargs -0 $hlint; then
      echo 'FAILURE: HLint reported issues'
      echo 'Run "eval $(nix-build -A fix)" to attempt to fix this, manual fixes may be necessary'
      exit 1
  fi
  echo 'SUCCESS: HLint reported no issues'
''

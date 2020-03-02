{ apply-refact
, cabal-install
, hlint
, pkgs
, writeScript
}:

writeScript "fix.sh" ''
  set -euo pipefail
  IFS=$'\n\t'

  cabal="${cabal-install}/bin/cabal"
  git="${pkgs.git}/bin/git"
  hlint="${hlint}/bin/hlint"
  ormolu="${pkgs.ormolu}/bin/ormolu"
  refactor="${apply-refact}/bin/refactor"

  if ! $git diff --quiet -- *.cabal; then
      echo 'ERROR: Dirty working directory'
      exit 1
  fi

  $cabal format *.cabal
  echo 'SUCCESS: Formatted cabal file'

  function haskell_files {
      $git ls-tree -z -r HEAD --name-only | grep -z '\.hs$'
  }

  haskell_files | xargs -0 -I{} $hlint {} --refactor --with-refactor=$refactor --refactor-options="--inplace"
  echo 'SUCCESS: Applied HLint suggestions'

  haskell_files | xargs -0 -I{} $ormolu --mode inplace {}
  echo 'SUCCESS: Formatted Haskell files with Ormolu'
''

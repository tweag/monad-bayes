{ nix-prefetch-git
, writeScript
}:

writeScript "update.sh" ''
  set -euo pipefail
  IFS=$'\n\t'

  nixprefetchgit="${nix-prefetch-git}/bin/nix-prefetch-git"

  channel='nixos-unstable'
  nix-prefetch-git https://github.com/nixos/nixpkgs-channels.git --rev "refs/heads/$channel" > ./nix/nixpkgs.json
  nix-prefetch-git https://github.com/input-output-hk/haskell.nix.git --rev "refs/heads/master" > ./nix/haskell-nix.json
''

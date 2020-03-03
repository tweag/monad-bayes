{ bash
, nix-prefetch-git
, writeScript
}:

writeScript "update.sh" ''
  #!/usr/bin/env ${bash}/bin/bash

  set -euo pipefail
  IFS=$'\n\t'

  nixprefetchgit="${nix-prefetch-git}/bin/nix-prefetch-git"

  channel='nixos-20.03'
  nix-prefetch-git https://github.com/nixos/nixpkgs-channels.git --rev "refs/heads/$channel" > ./nix/nixpkgs.json
  nix-prefetch-git https://github.com/input-output-hk/haskell.nix.git --rev "refs/heads/master" > ./nix/haskell-nix.json
''

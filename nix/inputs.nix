# uses flake-compat to get the inputs from flake.lock
{ system ? builtins.currentSystem }:

let
  flake-compat =
    let
      lock = builtins.fromJSON (builtins.readFile ../flake.lock);
    in fetchTarball {
      url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
      sha256 = lock.nodes.flake-compat.locked.narHash;
    };
  flake = import flake-compat { inherit system; src = ./..; };
in flake.defaultNix.inputs

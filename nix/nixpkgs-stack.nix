# stack expects nix.path.nixpkgs to point to a function, hence this wrapper.
_args: import ./nixpkgs.nix { }

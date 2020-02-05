with import <nixpkgs> {};

mkShell {
  buildInputs = [
    # Haskell
    haskellPackages.hindent
    hlint
    stack
    stylish-cabal

    # Python
    python37
    python37Packages.matplotlib
    python37Packages.pandas

    # Nix
    nixpkgs-fmt
  ];
}

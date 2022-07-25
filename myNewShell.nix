let
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/22.05.tar.gz";
    sha256 = "sha256:0d643wp3l77hv2pmg2fi7vyxn4rwy0iyr8djcw1h5x72315ck9ik";
  };

  # pkgs= builtins.fetchGit {
  #   # Descriptive name to make the store path easier to identify
  #   name = "nixos-unstable-2018-09-12";
  #   url = "https://github.com/nixos/nixpkgs/";
  #   ref = "refs/heads/master";
  #   rev = "fe237597d151a33b6aab54f1f5a0af6353c74d04";
  # };

  myHaskellPackageOverlay = self: super: {
    myHaskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper: rec {
        # random-fu = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
        #   hself.callCabal2nixWithOptions "random-fu" (builtins.fetchGit {
        #     url = "https://github.com/lehins/random-fu";
        #     rev = "1eb5c0080618a691cd9fa81ac3d0ea29edeb084f";
        #     ref = "switch-to-random";
        #   }) "--subpath random-fu" { }
        # ))) [ ];

        # rvar = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
        #   hself.callCabal2nixWithOptions "rvar" (builtins.fetchGit {
        #     url = "https://github.com/lehins/random-fu";
        #     rev = "1eb5c0080618a691cd9fa81ac3d0ea29edeb084f";
        #     ref = "switch-to-random";
        #   }) "--subpath rvar" { }
        # ))) [ ];

        # random = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
        #   hself.callCabal2nixWithOptions "rvar" (builtins.fetchGit {
        #     url = "https://github.com/haskell/random";
        #     rev = "edae4f7908f3c7e00be1094034a4a09cd72ab35e";
        #   }) "" { }
        # ))) [ ];

        # hashable = super.haskell.lib.doJailbreak hsuper.hashable;

        mwc-random = hself.callHackage "mwc-random" "0.15.0.1" {};

        # brick = hself.callHackage "brick" "0.69" {};

        brick = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
          hself.callCabal2nixWithOptions "brick" (builtins.fetchGit {
            url = "https://github.com/jtdaugherty/brick";
            rev = "d196695ab7fee3e1b27e2b34ae7d96d369fe1723";
          }) "" {}
        ))) [];

        # random-fu-multivariate =  hsuper.random-fu-multivariate;

        hmatrix-sundials = super.haskell.lib.doJailbreak (
          super.haskell.lib.dontCheck (
            hself.callCabal2nix "hmatrix-sundials" (builtins.fetchGit {
              url = "https://github.com/novadiscovery/hmatrix-sundials";
              rev = "76bfee5b5a8377dc3f7161514761946a60d4834a";
            }) {
              sundials_arkode = self.sundials;
              sundials_cvode = self.sundials;
              klu = self.suitesparse;
              suitesparseconfig = self.suitesparse;
              sundials_sunlinsolklu = self.sundials;
              sundials_sunmatrixsparse = self.sundials;
            }
          )
        );

        hmatrix = self.haskell.lib.addBuildDepends (super.haskell.lib.doJailbreak (
          super.haskell.lib.dontCheck (
            hself.callCabal2nixWithOptions "hmatrix" (builtins.fetchGit {
              url = "https://github.com/haskell-numerics/hmatrix";
              rev = "2e957f417a36a33a86953f291f4629b6942513f9";
            }) "--subpath packages/base" {}
          )
        )) [self.darwin.apple_sdk.frameworks.Accelerate];
      };
    };
  };
in
  {
    nixpkgs ?
      import pkgs
      {
        config.allowBroken = true;
        overlays = [myHaskellPackageOverlay];
      },
    compiler ? "default",
    doBenchmark ? false,
  }: let
    cabal-docspec = import ./nix/cabal-docspec.nix {};

    haskellDeps = ps:
      with ps; [
        base
        brick
        derive-storable
        free
        generic-deriving
        hmatrix
        ieee754
        log-domain
        mtl
        monad-coroutine
        mwc-random
        random
        random-fu
        random-fu-multivariate
        statistics
        vector
        cabal-docspec
        foldl
        histogram-fill
        matrix
        microlens-ghc
        mmorph
        integration
        prettyprinter
        pipes
        prettyprinter-ansi-terminal
        pretty-simple
        hvega
        (nixpkgs.haskell.lib.dontCheck criterion)

        hmatrix-sundials
        linear

        matrix

        reflection
        ad

        Chart
        Chart-diagrams
        diagrams-svg
        histogram-fill

        typed-process

        cabal-doctest
      ];
  in
    nixpkgs.stdenv.mkDerivation {
      name = "env";
      buildInputs = with nixpkgs.rPackages; [
        nixpkgs.cabal-install
        nixpkgs.ormolu
        nixpkgs.doctest
        cabal-docspec
        (nixpkgs.myHaskellPackages.ghcWithPackages haskellDeps)
      ];
    }

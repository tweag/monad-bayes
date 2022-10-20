let

myHaskellPackageOverlay = self: super: {
  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {

    };
  };
};

in

{ nixpkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixpkgs-22.05-darwin.tar.gz")
  {
    overlays = [ myHaskellPackageOverlay ];
  }
}:

let
  R-with-my-packages = nixpkgs.rWrapper.override{
    packages = with nixpkgs.rPackages; [
      ggplot2
      reshape2
      tidyverse
      jqr
      RPostgres
      readxl
      openxlsx
    ]; };

  pkgs = nixpkgs;

  haskellDeps = ps: with ps; [
    ad
    base
    cassava
    hasql
    http-client
    http-client-tls
    http-conduit
    microlens
    monad-extras
    path
    path-io
    pipes
    postgresql-binary
    process
    unix-compat
    vinyl
    xlsx
    zip
    base brick containers foldl free histogram-fill ieee754 integration
    lens linear log-domain math-functions matrix monad-coroutine
    monad-extras mtl mwc-random pipes pretty-simple primitive random
    safe scientific statistics text transformers vector vty

    Chart
    Chart-diagrams
    diagrams-svg
    diagrams-rasterific
  ];

in

pkgs.stdenv.mkDerivation {
  name = "rOnly";

  buildInputs = [
    pkgs.libintlOrEmpty
    R-with-my-packages
    pkgs.postgresql_13
    pkgs.lhs2tex
    (pkgs.myHaskellPackages.ghcWithPackages haskellDeps)
    pkgs.darwin.apple_sdk.frameworks.Cocoa
  ];
}

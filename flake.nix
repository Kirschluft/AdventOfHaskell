{
  description = "A Nix flake for GHC 9.2 and Haskell Language Server";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc92;
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.zlib
            haskellPackages.ghc
            haskellPackages.cabal-install
            (haskellPackages.haskell-language-server.override {
              ghc = haskellPackages.ghc;
            })
          ];
        };
      }
    );
}

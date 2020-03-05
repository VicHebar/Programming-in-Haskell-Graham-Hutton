# {nixpkgs ? import <nixpkgs> { config = import ./config.nix; }, ghc ? nixpkgs.ghc}:
{nixpkgs ? import <nixpkgs> {}, ghc ? nixpkgs.ghc}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "laurus-nobilis";
  buildInputs = [ zlib
                  gmp
                  git
                ];
  inherit ghc;
}

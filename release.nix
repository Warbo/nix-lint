# Used for building and testing on Hydra

# Provides a bunch of nixpkgs versions, augmented with useful helper functions
with builtins;
with {
  helpersSrc = (import <nixpkgs> {}).fetchgit {
    url    = http://chriswarbo.net/git/nix-helpers.git;
    rev    = "9e86df4";
    sha256 = "0s2b87q0bifkga514jhxf3cb0asyjhpjyhdpqbsfiinilxy7xih4";
  };
};

with import helpersSrc;
with { hsPkgs = nixpkgs1709.haskell.packages.ghc802; };
{
  "nixpkgs1709.haskell.packages.ghc802.nix-lint" =
    hsPkgs.callPackage (nixpkgs1803.haskellPackages.haskellSrc2nix {
                         name = "nix-lint";
                         src  = ./.;
                       })
                       {};
}

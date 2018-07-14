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
collapseAttrs (haskellRelease {
  name        = "nix-lint";
  dir         = ./.;
  hackageSets = { nixpkgs1709 = [ "ghc802" ]; };
})

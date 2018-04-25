# Used for building and testing on Hydra

# Provides a bunch of nixpkgs versions, augmented with useful helper functions
with rec {
  pkgSrc = (import <nixpkgs> { config = {}; }).fetchgit {
    url    = http://chriswarbo.net/git/nix-config.git;
    rev    = "74ba11d";
    sha256 = "0lzks69z5kfj5akd7f32z86sgnbld56976zcn290yj987nlq6g2m";
  };

  unstablePkgs = import pkgSrc {};
};

with builtins;
with unstablePkgs.nixpkgs1709.lib;
with rec {
  buildForNixpkgs = nixVersion:
    with { super = getAttr nixVersion unstablePkgs; };
    if compareVersions nixVersion "nixpkgs1703" == -1
       then ""
       else ''
         #
           "${nixVersion}" = with ${nixVersion}.haskell; {
             ${concatStringsSep "\n" (map buildForHaskell
                                          (attrNames super.haskell.packages))}
           };
       '';

  buildForHaskell = hsVersion:
    if hasPrefix "ghcjs"          hsVersion || hasPrefix "lts"      hsVersion ||
       hasPrefix "ghc6"           hsVersion || hasPrefix "ghc7"     hsVersion ||
       hasPrefix "ghcHaLVM"       hsVersion || hasSuffix "HEAD"     hsVersion ||
       hasPrefix "integer-simple" hsVersion || hasPrefix "ghcCross" hsVersion
       then ""
       else ''
         #
             "${hsVersion}" = go { haskellPackages = packages."${hsVersion}"; };
       '';

  pkgExpr =
    with unstablePkgs.customised.nixpkgs1709;
    runCommand "nix-lint-expr"
      {
        # A bare function, which we'll give arguments from the nixpkgs Haskell set
        nixpkgsDeps = pkgs.runCabal2nix { url = ./.; };

        # Uses a Cabal sandbox to pick dependencies from (a snapshot of) Hackage
        hackageDeps = pkgs.haskellPkgDepsDrv { dir = ./.; name = "nix-lint"; };

        default = pkgs.writeScript "default.nix" ''
          { haskellPackages }:

          with builtins;
          with (import <nixpkgs> { config = {}; }).lib;
          with rec {
            depNames = import ./deps;

            overrides = self: super: genAttrs depNames
              (n: self.callPackage (./deps/pkgs + "/${"$" + "{n}"}.nix") {});

            hsPkgs = haskellPackages.override { inherit overrides; };
          };
          {
            hackageDeps = hsPkgs.nix-lint;
            nixpkgsDeps = haskellPackages.callPackage ./fromCabal2nix.nix {};
          }
        '';
      }
      ''
        mkdir "$out"
        cp -r "$hackageDeps" "$out/deps"
        cp    "$nixpkgsDeps" "$out/fromCabal2nix.nix"
        cp "$default" "$out/default.nix"
      '';
};

with unstablePkgs.customised.nixpkgs1709;
attrsToDirs {
  "default.nix" = writeScript "nix-lint-release.nix" ''
    with import ${pkgSrc} {};
    with { go = import ${pkgExpr}; };
    {
      ${concatStringsSep "\n" (map buildForNixpkgs
                                   (attrNames unstablePkgs.customised))}
    }
  '';
}

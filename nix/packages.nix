{
  pkgs,
  compiler,
}: let
  lib = pkgs.lib;

  util = import ./util.nix {
    inherit pkgs;
    inherit (pkgs) lib gitignoreFilter;
  };

  conf = lib.importTOML ../nixkell.toml;

  ghcVersion =
    if compiler != null
    then compiler
    else conf.ghc;

  ourHaskell = pkgs.haskell.packages.${"ghc" + util.removeChar "." ghcVersion}.override {
    overrides = let
      depsFromDir = pkgs.haskell.lib.packagesFromDirectory {
        directory = ./packages;
      };
      manual = _hfinal: hprev: {
        aoc2022 = let
          filteredSrc = util.filterSrc ../. {
            ignoreFiles = conf.ignore.files;
            ignorePaths = conf.ignore.paths;
          };
        in
          (hprev.callCabal2nix "aoc2022" filteredSrc {}).overrideAttrs (prev: {
            preCheck = ''
              export AOC_INPUT_PATH=$out/input

              # Note: On the GH actions Linux runner $out did not exist at this point
              # hence creating it. On the MacOS runner and locally it does exist.
              mkdir -vp $AOC_INPUT_PATH

              if [[ -n "$(ls --almost-all --ignore=.gitkeep input)" ]]; then
                cp -vr input/* $AOC_INPUT_PATH
              fi
            '';
          });
      };
    in
      lib.composeExtensions depsFromDir manual;
  };

  ghc = ourHaskell.ghc.withPackages (
    _ps: pkgs.haskell.lib.getHaskellBuildInputs ourHaskell.aoc2022
  );

  tools = let
    getDrvUsingOurHaskell = p: ourHaskell.${lib.removePrefix "haskellPackages." p};
  in
    map getDrvUsingOurHaskell conf.env.haskell_tools ++ map util.getDrv conf.env.tools;

  scripts = import ./scripts.nix {inherit pkgs;};
in {
  bin = util.leanPkg ourHaskell.aoc2022;

  shell = pkgs.buildEnv {
    name = "aoc2022-env";
    paths = [ghc] ++ tools ++ scripts;
  };
}

{
  system ? builtins.currentSystem,
  compiler ? null,
}: let
  pkgs = import ./nix {inherit system compiler;};
in
  pkgs.mkShell {
    buildInputs = [
      pkgs.aoc2022.shell
    ];
    shellHook = ''
      export LD_LIBRARY_PATH=${pkgs.aoc2022.shell}/lib:$LD_LIBRARY_PATH
      logo
    '';
  }

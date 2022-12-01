{
  sources,
  compiler,
}: [
  (final: prev: {
    inherit (import sources.gitignore {inherit (prev) lib;}) gitignoreFilter;
  })
  (final: prev: {
    aoc2022 = import ./packages.nix {
      pkgs = prev;
      inherit compiler;
    };
  })
]

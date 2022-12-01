#!/usr/bin/env bash
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

shell_scripts=()
for file in $(git ls-files | grep -e '\.sh$'); do
  shell_scripts+=("$file")
done
shellcheck --norc "${shell_scripts[@]}"

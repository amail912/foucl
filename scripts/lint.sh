#!/usr/bin/env bash
set -euo pipefail

if ! command -v hlint >/dev/null 2>&1; then
  echo "hlint is not installed."
  echo "Install it with: cabal install hlint"
  exit 1
fi

hlint src app test --hint=.hlint.yaml

#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
for file in _quarto.yml index.qmd data/example/independent-groups.csv; do
  test -s "$file" || { printf 'Missing: %s\n' "$file"; exit 1; }
done
for tool in quarto Rscript python3; do
  if command -v "$tool" >/dev/null 2>&1; then
    printf 'Available: %s\n' "$tool"
  else
    printf 'Not installed: %s\n' "$tool"
  fi
done

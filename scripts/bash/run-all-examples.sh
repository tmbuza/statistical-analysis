#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
command -v Rscript >/dev/null || { echo "Rscript is required." >&2; exit 1; }
command -v python >/dev/null || { echo "Activate the project Python environment." >&2; exit 1; }
python scripts/python/03-check-data.py
for script in scripts/R/*.R; do
  echo "Running $script"
  Rscript "$script"
done

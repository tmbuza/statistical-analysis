#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
command -v quarto >/dev/null 2>&1 || { printf '%s\n' 'Install Quarto before rendering.'; exit 1; }
quarto render

#!/usr/bin/env bash
set -euo pipefail

# Render the supplemental material PDF independently.
# Must be run from the project root: bash pipeline/run_supplement.sh
# Requires run_manuscript.sh (or individual figure scripts) to have been run first.

TIMESTAMP="$(date +%Y-%m-%d_%H-%M-%S)"
_PARENT="${PIPELINE_LOG_DIR:-logs}"
LOG_DIR="${_PARENT}/run_supplement_${TIMESTAMP}"
mkdir -p output/manuscript "$LOG_DIR"
echo "Logging to: ${LOG_DIR}"

echo "=== Rendering supplemental material ==="
quarto render src/manuscript/supplemental-material.qmd \
  > "${LOG_DIR}/supplemental-material_$(date +%H-%M-%S).txt" 2>&1
mv src/manuscript/supplemental-material.pdf output/manuscript/

echo "=== Done: output/manuscript/supplemental-material.pdf ==="

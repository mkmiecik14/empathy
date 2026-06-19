#!/usr/bin/env bash
set -euo pipefail

# Run a single src/manuscript/ R script with the same logging as run_manuscript.sh.
# Must be run from the project root: bash pipeline/run_manuscript_script.sh <script-name>
# <script-name> is the base name with or without .R, e.g.:
#   bash pipeline/run_manuscript_script.sh manu-mixed-model-results
#   bash pipeline/run_manuscript_script.sh manu-mixed-model-results.R

if [[ $# -lt 1 ]]; then
  echo "Usage: bash pipeline/run_manuscript_script.sh <script-name>"
  echo "  Available scripts:"
  ls src/manuscript/*.R | xargs -n1 basename | sed 's/^/    /'
  exit 1
fi

SCRIPT_NAME="${1%.R}"   # strip .R if provided
SCRIPT_PATH="src/manuscript/${SCRIPT_NAME}.R"

if [[ ! -f "$SCRIPT_PATH" ]]; then
  echo "Error: script not found: ${SCRIPT_PATH}"
  echo "  Available scripts:"
  ls src/manuscript/*.R | xargs -n1 basename | sed 's/^/    /'
  exit 1
fi

TIMESTAMP="$(date +%Y-%m-%d_%H-%M-%S)"
_PARENT="${PIPELINE_LOG_DIR:-logs}"
LOG_DIR="${_PARENT}/run_manuscript_${TIMESTAMP}"
mkdir -p output/manuscript "$LOG_DIR"
echo "Logging to: ${LOG_DIR}"

echo "=== Running ${SCRIPT_NAME}.R ==="
Rscript "$SCRIPT_PATH" \
  > "output/manuscript/${SCRIPT_NAME}-console.txt" \
  2> "${LOG_DIR}/${SCRIPT_NAME}_$(date +%H-%M-%S).txt"

echo "=== Done: output/manuscript/${SCRIPT_NAME}-console.txt ==="

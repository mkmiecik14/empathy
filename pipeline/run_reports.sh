#!/usr/bin/env bash
set -euo pipefail

# Render all analysis reports.
# Must be run from the project root: bash pipeline/run_reports.sh
# Requires run_analyses.sh to have been run first.

TIMESTAMP="$(date +%Y-%m-%d_%H-%M-%S)"
_PARENT="${PIPELINE_LOG_DIR:-logs}"
LOG_DIR="${_PARENT}/run_reports_${TIMESTAMP}"
mkdir -p output/reports "$LOG_DIR"
echo "Logging to: ${LOG_DIR}"

echo "=== Rendering analysis reports ==="
for qmd in src/reports/*.qmd; do
  base="$(basename "$qmd")"
  [[ "$base" == "template-report.qmd" ]] && continue
  echo "Rendering $qmd ..."
  quarto render "$qmd" \
    > "${LOG_DIR}/${base%.qmd}_$(date +%H-%M-%S).txt" 2>&1
  pdf="${qmd%.qmd}.pdf"
  if [[ -f "$pdf" ]]; then
    mv "$pdf" output/reports/
  fi
done

echo "=== Reports complete ==="

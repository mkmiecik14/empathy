#!/usr/bin/env bash
set -euo pipefail

# Render all reports and generate manuscript outputs.
# Must be run from the project root: bash pipeline/run_manuscript.sh
# Requires run_analyses.sh to have been run first.

TIMESTAMP="$(date +%Y-%m-%d_%H-%M-%S)"
_PARENT="${PIPELINE_LOG_DIR:-logs}"
LOG_DIR="${_PARENT}/run_manuscript_${TIMESTAMP}"
mkdir -p output/manuscript "$LOG_DIR"
echo "Logging to: ${LOG_DIR}"

echo "=== Running manuscript R scripts ==="
Rscript src/manuscript/manu-table-1.R \
  > "output/manuscript/manu-table-1-console.txt" \
  2> "${LOG_DIR}/manu-table-1_$(date +%H-%M-%S).txt"
Rscript src/manuscript/manu-gamm-results.R \
  > "output/manuscript/manu-gamm-results-console.txt" \
  2> "${LOG_DIR}/manu-gamm-results_$(date +%H-%M-%S).txt"
Rscript src/manuscript/manu-pca-results.R \
  > "output/manuscript/manu-pca-results-console.txt" \
  2> "${LOG_DIR}/manu-pca-results_$(date +%H-%M-%S).txt"
Rscript src/manuscript/manu-consort.R \
  > "output/manuscript/manu-consort-console.txt" \
  2> "${LOG_DIR}/manu-consort_$(date +%H-%M-%S).txt"
Rscript src/manuscript/manu-bladder-task-imputation-info.R \
  > "output/manuscript/manu-bladder-task-imputation-info-console.txt" \
  2> "${LOG_DIR}/manu-bladder-task-imputation-info_$(date +%H-%M-%S).txt"
Rscript src/manuscript/supplemental-boxplots.R \
  > "output/manuscript/supplemental-boxplots-console.txt" \
  2> "${LOG_DIR}/supplemental-boxplots_$(date +%H-%M-%S).txt"

echo "=== Rendering supplemental material ==="
quarto render src/manuscript/supplemental-material.qmd \
  > "${LOG_DIR}/supplemental-material_$(date +%H-%M-%S).txt" 2>&1
mv src/manuscript/supplemental-material.pdf output/manuscript/

echo "=== Manuscript complete ==="

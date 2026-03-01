#!/usr/bin/env bash
set -euo pipefail

# Render all reports and generate manuscript outputs.
# Must be run from the project root: bash pipeline/run_manuscript.sh
# Requires run_analyses.sh to have been run first.

TIMESTAMP="$(date +%Y-%m-%d_%H-%M-%S)"
_PARENT="${PIPELINE_LOG_DIR:-logs}"
LOG_DIR="${_PARENT}/run_manuscript_${TIMESTAMP}"
mkdir -p output/reports output/manuscript "$LOG_DIR"
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

echo "=== Running manuscript R scripts ==="
Rscript src/manuscript/manu-table-1.R \
  > "${LOG_DIR}/manu-table-1_$(date +%H-%M-%S).txt" 2>&1
Rscript src/manuscript/manu-gamm-results.R \
  > "${LOG_DIR}/manu-gamm-results_$(date +%H-%M-%S).txt" 2>&1
Rscript src/manuscript/manu-pca-results.R \
  > "${LOG_DIR}/manu-pca-results_$(date +%H-%M-%S).txt" 2>&1
Rscript src/manuscript/manu-consort.R \
  > "${LOG_DIR}/manu-consort_$(date +%H-%M-%S).txt" 2>&1
Rscript src/manuscript/manu-bladder-task-imputation-info.R \
  > "${LOG_DIR}/manu-bladder-task-imputation-info_$(date +%H-%M-%S).txt" 2>&1
Rscript src/manuscript/supplemental-boxplots.R \
  > "${LOG_DIR}/supplemental-boxplots_$(date +%H-%M-%S).txt" 2>&1

echo "=== Rendering supplemental material ==="
quarto render src/manuscript/supplemental-material.qmd \
  > "${LOG_DIR}/supplemental-material_$(date +%H-%M-%S).txt" 2>&1
mv src/manuscript/supplemental-material.pdf output/manuscript/

echo "=== Manuscript complete ==="

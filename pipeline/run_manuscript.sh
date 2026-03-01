#!/usr/bin/env bash
set -euo pipefail

# Render all reports and generate manuscript outputs.
# Must be run from the project root: bash pipeline/run_manuscript.sh
# Requires run_analyses.sh to have been run first.

mkdir -p output/reports output/manuscript

echo "=== Rendering analysis reports ==="
for qmd in src/reports/*.qmd; do
  base="$(basename "$qmd")"
  [[ "$base" == "template-report.qmd" ]] && continue
  echo "Rendering $qmd ..."
  quarto render "$qmd"
  pdf="${qmd%.qmd}.pdf"
  if [[ -f "$pdf" ]]; then
    mv "$pdf" output/reports/
  fi
done

echo "=== Running manuscript R scripts ==="
Rscript src/manuscript/manu-table-1.R
Rscript src/manuscript/manu-gamm-results.R
Rscript src/manuscript/manu-pca-results.R
Rscript src/manuscript/manu-consort.R
Rscript src/manuscript/manu-bladder-task-imputation-info.R
Rscript src/manuscript/supplemental-boxplots.R

echo "=== Rendering supplemental material ==="
quarto render src/manuscript/supplemental-material.qmd
mv src/manuscript/supplemental-material.pdf output/manuscript/

echo "=== Manuscript complete ==="

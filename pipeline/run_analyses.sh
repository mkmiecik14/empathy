#!/usr/bin/env bash
set -euo pipefail

# Run all analysis scripts in dependency order.
# Must be run from the project root: bash pipeline/run_analyses.sh
# Requires run_preprocessing.sh to have been run first.

TIMESTAMP="$(date +%Y-%m-%d_%H-%M-%S)"
_PARENT="${PIPELINE_LOG_DIR:-logs}"
LOG_DIR="${_PARENT}/run_analyses_${TIMESTAMP}"
mkdir -p "$LOG_DIR"
echo "Logging to: ${LOG_DIR}"

echo "=== PCA analyses ==="
Rscript src/analysis/analysis-pca-1.R \
  > "${LOG_DIR}/analysis-pca-1_$(date +%H-%M-%S).txt" 2>&1
Rscript src/analysis/analysis-pca-2.R \
  > "${LOG_DIR}/analysis-pca-2_$(date +%H-%M-%S).txt" 2>&1
Rscript src/analysis/analysis-pca-3.R \
  > "${LOG_DIR}/analysis-pca-3_$(date +%H-%M-%S).txt" 2>&1
Rscript src/analysis/analysis-pca-correlations.R \
  > "${LOG_DIR}/analysis-pca-correlations_$(date +%H-%M-%S).txt" 2>&1

echo "=== Longitudinal pain analysis ==="
Rscript src/analysis/analysis-long-pain.R \
  > "${LOG_DIR}/analysis-long-pain_$(date +%H-%M-%S).txt" 2>&1
Rscript src/analysis/analysis-long-mmh.R \
  > "${LOG_DIR}/analysis-long-mmh_$(date +%H-%M-%S).txt" 2>&1
Rscript src/analysis/analysis-long-mmh-mod-comp.R \
  > "${LOG_DIR}/analysis-long-mmh-mod-comp_$(date +%H-%M-%S).txt" 2>&1

echo "=== Supplementary projection preprocessing (depends on PCA-3 results) ==="
Rscript src/prepro/prepro-long-supp-proj.R \
  > "${LOG_DIR}/prepro-long-supp-proj_$(date +%H-%M-%S).txt" 2>&1

echo "=== Supplementary projection analyses ==="
Rscript src/analysis/analysis-long-supp-proj.R \
  > "${LOG_DIR}/analysis-long-supp-proj_$(date +%H-%M-%S).txt" 2>&1
Rscript src/analysis/analysis-long-supp-proj-simplified.R \
  > "${LOG_DIR}/analysis-long-supp-proj-simplified_$(date +%H-%M-%S).txt" 2>&1

echo "=== Analyses complete ==="

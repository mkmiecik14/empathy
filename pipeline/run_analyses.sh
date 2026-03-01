#!/usr/bin/env bash
set -euo pipefail

# Run all analysis scripts in dependency order.
# Must be run from the project root: bash pipeline/run_analyses.sh
# Requires run_preprocessing.sh to have been run first.

echo "=== PCA analyses ==="
Rscript src/analysis/analysis-pca-1.R
Rscript src/analysis/analysis-pca-2.R
Rscript src/analysis/analysis-pca-3.R
Rscript src/analysis/analysis-pca-correlations.R

echo "=== Longitudinal pain analysis ==="
Rscript src/analysis/analysis-long-pain.R
Rscript src/analysis/analysis-long-mmh.R
Rscript src/analysis/analysis-long-mmh-mod-comp.R

echo "=== Supplementary projection preprocessing (depends on PCA-3 results) ==="
Rscript src/prepro/prepro-long-supp-proj.R

echo "=== Supplementary projection analyses ==="
Rscript src/analysis/analysis-long-supp-proj.R
Rscript src/analysis/analysis-long-supp-proj-simplified.R

echo "=== Analyses complete ==="

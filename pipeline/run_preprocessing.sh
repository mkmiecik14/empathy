#!/usr/bin/env bash
set -euo pipefail

# Run all preprocessing scripts in dependency order.
# Must be run from the project root: bash pipeline/run_preprocessing.sh

mkdir -p output/prepro

echo "=== Layer 0: scripts with no upstream dependencies ==="
Rscript src/prepro/prepro-ss-masterlist.R
Rscript src/prepro/prepro-hsc-data.R
Rscript src/prepro/prepro-gss-tanner.R
Rscript src/prepro/prepro-vis-task.R
Rscript src/prepro/prepro-aud-task.R
Rscript src/prepro/prepro-vis-acuity.R
Rscript src/prepro/prepro-ppt.R
Rscript src/prepro/prepro-cold-pain.R
Rscript src/prepro/prepro-afterpain.R

echo "=== Layer 1: join task and PPT data with REDCap ==="
Rscript src/prepro/redcap_join.R

echo "=== Layer 2: scripts that depend on the joined REDCap data ==="
Rscript src/prepro/prepro-cssi-data.R
Rscript src/prepro/prepro-bladder-task.R
Rscript src/prepro/prepro-long-pain.R

echo "=== Layer 3: scripts that depend on Layer 2 ==="
Rscript src/prepro/impute-bladder-task.R
Rscript src/prepro/prepro-pca-data.R

echo "=== Preprocessing complete ==="

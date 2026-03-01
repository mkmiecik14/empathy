#!/usr/bin/env bash
set -euo pipefail

# Run all preprocessing scripts in dependency order.
# Must be run from the project root: bash pipeline/run_preprocessing.sh

TIMESTAMP="$(date +%Y-%m-%d_%H-%M-%S)"
_PARENT="${PIPELINE_LOG_DIR:-logs}"
LOG_DIR="${_PARENT}/run_preprocessing_${TIMESTAMP}"
mkdir -p output/prepro "$LOG_DIR"
echo "Logging to: ${LOG_DIR}"

echo "=== Layer 0: scripts with no upstream dependencies ==="
Rscript src/prepro/prepro-hsc-data.R \
  > "${LOG_DIR}/prepro-hsc-data_$(date +%H-%M-%S).txt" 2>&1
Rscript src/prepro/prepro-gss-tanner.R \
  > "${LOG_DIR}/prepro-gss-tanner_$(date +%H-%M-%S).txt" 2>&1
Rscript src/prepro/prepro-vis-task.R \
  > "${LOG_DIR}/prepro-vis-task_$(date +%H-%M-%S).txt" 2>&1
Rscript src/prepro/prepro-aud-task.R \
  > "${LOG_DIR}/prepro-aud-task_$(date +%H-%M-%S).txt" 2>&1
Rscript src/prepro/prepro-vis-acuity.R \
  > "${LOG_DIR}/prepro-vis-acuity_$(date +%H-%M-%S).txt" 2>&1
Rscript src/prepro/prepro-ppt.R \
  > "${LOG_DIR}/prepro-ppt_$(date +%H-%M-%S).txt" 2>&1

echo "=== Layer 1: join task and PPT data with REDCap ==="
Rscript src/prepro/redcap_join.R \
  > "${LOG_DIR}/redcap_join_$(date +%H-%M-%S).txt" 2>&1

echo "=== Layer 2: scripts that depend on the joined REDCap data ==="
Rscript src/prepro/prepro-cold-pain.R \
  > "${LOG_DIR}/prepro-cold-pain_$(date +%H-%M-%S).txt" 2>&1
Rscript src/prepro/prepro-afterpain.R \
  > "${LOG_DIR}/prepro-afterpain_$(date +%H-%M-%S).txt" 2>&1
Rscript src/prepro/prepro-cssi-data.R \
  > "${LOG_DIR}/prepro-cssi-data_$(date +%H-%M-%S).txt" 2>&1
Rscript src/prepro/prepro-bladder-task.R \
  > "${LOG_DIR}/prepro-bladder-task_$(date +%H-%M-%S).txt" 2>&1
Rscript src/prepro/prepro-long-pain.R \
  > "${LOG_DIR}/prepro-long-pain_$(date +%H-%M-%S).txt" 2>&1

echo "=== Layer 3: scripts that depend on Layer 2 ==="
Rscript src/prepro/impute-bladder-task.R \
  > "${LOG_DIR}/impute-bladder-task_$(date +%H-%M-%S).txt" 2>&1
Rscript src/prepro/prepro-pca-data.R \
  > "${LOG_DIR}/prepro-pca-data_$(date +%H-%M-%S).txt" 2>&1

echo "=== Preprocessing complete ==="

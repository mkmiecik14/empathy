#!/usr/bin/env bash
set -euo pipefail

# Run the full EMPATHY analysis pipeline from project root.
# Usage: bash pipeline/run_pipeline.sh

TIMESTAMP="$(date +%Y-%m-%d_%H-%M-%S)"
export PIPELINE_LOG_DIR="logs/run_pipeline_${TIMESTAMP}"
mkdir -p "$PIPELINE_LOG_DIR"
echo "Pipeline logging to: ${PIPELINE_LOG_DIR}"

bash pipeline/run_preprocessing.sh
bash pipeline/run_analyses.sh
bash pipeline/run_manuscript.sh

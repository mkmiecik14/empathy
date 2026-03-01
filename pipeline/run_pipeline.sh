#!/usr/bin/env bash
set -euo pipefail

# Run the full EMPATHY analysis pipeline from project root.
# Usage: bash pipeline/run_pipeline.sh

bash pipeline/run_preprocessing.sh
bash pipeline/run_analyses.sh
bash pipeline/run_manuscript.sh

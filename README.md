# EMPATHY

Research data analysis pipeline for the EMPATHY project at Endeavor Health. This project studies multimodal sensory hypersensitivity and pain using a combination of R statistical analysis, Quarto reports, and MATLAB EEG preprocessing.

**Reproducibility policy:** one script per analysis, tracked via git commits.

---

## Prerequisites

- **R** with the following packages:
  - Data: `tidyverse`, `Hmisc`, `readxl`, `R.matlab`
  - Statistics: `lme4`, `lmerTest`, `mgcv`, `MuMIn`, `brms`, `broomExtra`
  - Reporting: `quarto`, `glue`, `knitr`, `flextable`
  - Visualization: `ggplot2`, `patchwork`, `RColorBrewer`
- **Quarto CLI** — required for rendering reports
- **MATLAB** — required only for EEG preprocessing (see [EEG Processing](#eeg-processing))

---

## Project Structure

```
empathy/
├── pipeline/           # Bash orchestration scripts
├── src/
│   ├── prepro/         # Data preprocessing scripts
│   ├── analysis/       # Statistical analysis scripts (one per analysis)
│   ├── reports/        # Quarto (.qmd) report documents
│   ├── manuscript/     # Manuscript tables, figures, and supplemental material
│   ├── eeg/            # MATLAB EEG processing scripts
│   └── misc/           # Miscellaneous helper scripts
├── fns/                # Shared R utility functions (sourced by all scripts)
├── data/               # Raw data (CSV, Excel, E-Prime .edat2 files)
├── output/             # Generated outputs
│   ├── prepro/         # Preprocessed RDS files + joined REDCap CSV
│   ├── analysis/       # Analysis result RDS files
│   ├── reports/        # Rendered PDF reports
│   └── manuscript/     # Manuscript-ready tables/figures + supplemental PDF
├── doc/                # Data dictionaries and project documentation
└── logs/               # Timestamped logs from pipeline runs
```

---

## Running the Pipeline

All commands must be run from the **project root**.

### Full pipeline

```bash
bash pipeline/run_pipeline.sh
```

This runs all three sub-pipelines in order and writes logs to `logs/run_pipeline_<timestamp>/`.

### Individual sub-pipelines

```bash
bash pipeline/run_preprocessing.sh   # preprocessing only
bash pipeline/run_analyses.sh        # analyses only (requires preprocessing)
bash pipeline/run_manuscript.sh      # reports + manuscript (requires analyses)
```

---

## Pipeline Details

### Preprocessing (`run_preprocessing.sh`)

Scripts run in dependency order; outputs go to `output/prepro/`.

| Layer | Scripts |
|-------|---------|
| **Layer 0** — no upstream dependencies | `prepro-hsc-data`, `prepro-gss-tanner`, `prepro-vis-task`, `prepro-aud-task`, `prepro-vis-acuity`, `prepro-ppt` |
| **Layer 1** — join task/PPT data with REDCap | `redcap_join` |
| **Layer 2** — depend on joined REDCap data | `prepro-cold-pain`, `prepro-afterpain`, `prepro-cssi-data`, `prepro-bladder-task`, `prepro-long-pain` |
| **Layer 3** — depend on Layer 2 | `impute-bladder-task`, `prepro-pca-data` |

### Analyses (`run_analyses.sh`)

Scripts run in dependency order; outputs go to `output/analysis/`.

| Stage | Scripts |
|-------|---------|
| **PCA chain** | `analysis-pca-1`, `analysis-pca-2`, `analysis-pca-3`, `analysis-pca-correlations` |
| **Longitudinal pain + MMH** | `analysis-long-pain`, `analysis-long-mmh`, `analysis-long-mmh-mod-comp` |
| **Supp-proj preprocessing** (depends on PCA-3) | `prepro-long-supp-proj` |
| **Supplementary projection** | `analysis-long-supp-proj`, `analysis-long-supp-proj-simplified` |

### Manuscript (`run_manuscript.sh`)

1. Renders all `src/reports/*.qmd` files → PDFs moved to `output/reports/`
2. Runs manuscript R scripts → outputs to `output/manuscript/`:
   - `manu-table-1`, `manu-gamm-results`, `manu-pca-results`, `manu-consort`, `manu-bladder-task-imputation-info`, `supplemental-boxplots`
3. Renders `src/manuscript/supplemental-material.qmd` → `output/manuscript/supplemental-material.pdf`

---

## Output Files

| Path | Contents |
|------|----------|
| `output/prepro/` | Preprocessed RDS files (e.g., `prepro-long-pain.rds`, `pca-data.rds`) and joined REDCap CSV |
| `output/analysis/` | Analysis result RDS files (e.g., `analysis-long-supp-proj.rds`) |
| `output/reports/` | Rendered PDF reports, one per analysis |
| `output/manuscript/` | Manuscript-ready tables, figures, and `supplemental-material.pdf` |

---

## EEG Processing

MATLAB scripts for EEG preprocessing are in `src/eeg/`. This pipeline is separate from the bash pipeline above and is not run automatically.

To run: open MATLAB, set your working directory appropriately, and run `visual_full_pipeline.m`. See `src/eeg/workspace_prep.m` for workspace initialization.

---

## Development Notes

- **Working directory:** all scripts assume they are run from the project root
- **New analysis:** copy `src/analysis/analysis-template-script.R` as a starting point
- **Exploratory scripts** (not included in the pipeline): `analysis-1.R`, `analysis-2.R`, `analysis-bladder-task-gam-1.R`, `analysis-supp-proj-inf.R`, `visual-data-qa.R`, `analysis-visual-acuity-qc-1.R`, `analysis-visual-task-qc-1.R`
- **RStudio:** open `ns-proj.Rproj` for project-relative path support
- **Logging:** each pipeline run writes timestamped logs to `logs/`

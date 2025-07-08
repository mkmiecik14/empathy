# empathy
Processing scripts for the EMPATHY project at NorthShore University HealthSystem

# A note about reproducibility

After beginning the project, I changed how I processed these data through the pipeline and adopted a "one script" policy per analysis.

This means that each analysis has 1 script and is versioned according to checked commits via git.

To run an analysis script, use the terminal from the main project directory:
```
Rscript [insert path to script] [version]
```

For example:
```
Rscript src/analysis-1.R v1 # to run version 1
```

Subsequent updates to the script are made and committed via git and ran using another version if desired.

Reports that explain the methods, results, etc. of each analysis are similarly versioned. 1 report per analysis script; they are named identical except for the file extension. Reports are also versioned according to the analysis version and will accept identical versions.

To render these according to version, use the R script in `fns/versioned_quarto_render.R`.
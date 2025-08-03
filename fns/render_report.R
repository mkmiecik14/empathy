#!/usr/bin/env Rscript

# Command line interface for rendering versioned reports
# Usage: Rscript src/fns/render_report.R <report_name> <version>
# Example: Rscript src/fns/render_report.R analysis-long-supp-proj v1

# Source the rendering functions
source("src/fns/versioned_quarto_render.R")

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  cat("Usage: Rscript src/fns/render_report.R <report_name> <version>\n")
  cat("   or: Rscript src/fns/render_report.R --interactive\n")
  cat("   or: Rscript src/fns/render_report.R --all <version>\n")
  quit(status = 1)
}

if (args[1] == "--interactive") {
  render_interactive()
} else if (args[1] == "--all" && length(args) >= 2) {
  render_all_reports(args[2])
} else if (length(args) >= 2) {
  render_versioned_report(args[1], args[2])
} else {
  cat("Error: Insufficient arguments\n")
  quit(status = 1)
}
# Versioned Quarto Rendering Functions
# Matt Kmiecik

library(glue); library(quarto)

# Main rendering function
render_versioned_report <- function(report_name, version, output_dir = "src/reports") {
  
  # Validate inputs
  qmd_file <- glue("{output_dir}/{report_name}.qmd")
  if (!file.exists(qmd_file)) {
    stop(glue("Quarto file not found: {qmd_file}"))
  }
  
  # Define output paths
  filename <- glue("{report_name}-{version}.pdf")
  final_path <- file.path(output_dir, filename)
  
  # Render the document
  cat(glue("Rendering {report_name} version {version}...\n"))
  
  quarto::quarto_render(
    input = qmd_file,
    output_file = filename,
    execute_params = list(version = version),
    quiet = FALSE
  )
  
  # Move file to the correct directory
  if (file.exists(filename)) {
    file.rename(filename, final_path)
    cat(glue("Report saved to: {final_path}\n"))
  } else {
    stop("Rendering failed - output file not found")
  }
  
  return(final_path)
}

# Convenience function to render all available reports for a version
render_all_reports <- function(version, output_dir = "src/reports") {
  
  # Find all .qmd files in reports directory
  qmd_files <- list.files(output_dir, pattern = "\\.qmd$", full.names = FALSE)
  report_names <- gsub("\\.qmd$", "", qmd_files)
  
  cat(glue("Found {length(report_names)} reports to render:\n"))
  cat(paste("-", report_names, collapse = "\n"), "\n\n")
  
  # Render each report
  results <- list()
  for (report in report_names) {
    tryCatch({
      results[[report]] <- render_versioned_report(report, version, output_dir)
    }, error = function(e) {
      cat(glue("Failed to render {report}: {e$message}\n"))
      results[[report]] <- NA
    })
  }
  
  return(results)
}

# Interactive function to select report and version
render_interactive <- function() {
  
  output_dir <- "src/reports"
  
  # Get available reports
  qmd_files <- list.files(output_dir, pattern = "\\.qmd$", full.names = FALSE)
  report_names <- gsub("\\.qmd$", "", qmd_files)
  
  if (length(report_names) == 0) {
    stop("No .qmd files found in src/reports/")
  }
  
  # Show available reports
  cat("Available reports:\n")
  for (i in seq_along(report_names)) {
    cat(glue("{i}: {report_names[i]}\n"))
  }
  
  # Get user selection
  choice <- as.numeric(readline("Select report number: "))
  if (is.na(choice) || choice < 1 || choice > length(report_names)) {
    stop("Invalid selection")
  }
  
  selected_report <- report_names[choice]
  version <- readline("Enter version (e.g., v1): ")
  
  if (version == "") {
    stop("Version cannot be empty")
  }
  
  # Render
  render_versioned_report(selected_report, version, output_dir)
}

# Quick render function for current development
render_current <- function(report_name, output_dir = "src/reports") {
  render_versioned_report(report_name, "test", output_dir)
}

# Example usage (commented out):
# render_versioned_report("analysis-long-supp-proj", "v1")
# render_all_reports("v1") 
# render_interactive()
# render_current("analysis-long-supp-proj")

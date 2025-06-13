# how to render a quarto document 

library(glue); library(quarto)

# Define params and output path
q <- "analysis-long-pain"
version <- "v1"
filename <- glue("{q}-{version}.pdf")
output_dir <- "src/reports"
final_path <- file.path(output_dir, filename)

quarto::quarto_render(
  input = glue("{output_dir}/{q}.qmd"),
  output_file = filename,
  execute_params = list(version = version)
)

# Move file to the correct directory
file.rename(filename, final_path)

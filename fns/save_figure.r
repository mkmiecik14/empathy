# function for creating plots
# Matt Kmiecik

library(ggplot2)

save_figure <- function(f, p, w, h, units = "in", dpi = 300, both = TRUE) {
  
  # Validate inputs
  if (missing(f) || missing(p)) {
    stop("Both filename (f) and plot (p) must be provided")
  }
  
  # Extract file components
  base_name <- tools::file_path_sans_ext(f)
  file_ext <- tolower(tools::file_ext(f))
  
  # If extension provided, use base name; otherwise use full filename as base
  if (file_ext != "") {
    final_base <- base_name
  } else {
    final_base <- f
  }
  
  if (both) {
    # Create filenames for both formats
    png_file <- paste0(final_base, ".png")
    tiff_file <- paste0(final_base, ".tiff")
    
    # Save PNG
    ggsave(filename = png_file, plot = p, width = w, height = h, 
           units = units, dpi = dpi, device = "png")
    
    # Save TIFF with compression
    ggsave(filename = tiff_file, plot = p, width = w, height = h, 
           units = units, dpi = dpi, device = "tiff", compression = "lzw")
    
    cat("Files saved:\n", png_file, "\n", tiff_file, "\n")
    
  } else {
    # Save single file
    if (file_ext == "") {
      final_file <- paste0(f, ".png")  # Default to PNG
    } else {
      final_file <- f
    }
    
    ggsave(filename = final_file, plot = p, width = w, height = h, 
           units = units, dpi = dpi)
    
    cat("File saved:", final_file, "\n")
  }
}
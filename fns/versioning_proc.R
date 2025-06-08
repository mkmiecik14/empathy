# versioning_proc.R
# Matt Kmiecik
# Started 2025-06-08
# Purpose: function to facilitate versioning process of pipeline

versioning_proc <- 
  function(
    testing = FALSE, this_script = NULL, odir = "output", ...
    ){
    
    # package loading
    if (!requireNamespace("here", quietly = TRUE)) {
      stop("Package 'here' is required but not installed.")
    }
    
    args <- commandArgs(trailingOnly = TRUE) # grabs args from Rscript
    
    # looks to see if script is being currently developed
    if (testing) {
      version <- "test"
    } else{
      version <- ifelse(length(args) == 0, stop("version not specified"), args)
    }
    
    output_dir <- here::here(odir) # sets output dir
    
    # checks if script name was provided, assigns
    if (is.null(this_script)) {
      stop("script name not provided")
    } else{
      script <- this_script
    }
    
    # returns
    r <- as.list(c(version, output_dir, script))
    names(r) <- c("version", "output", "script")
    return(r)
}

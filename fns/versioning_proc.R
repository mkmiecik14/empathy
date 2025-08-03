# versioning_proc.R
# Matt Kmiecik
# Started 2025-06-08
# Purpose: functions to facilitate versioning process of pipeline

# function to extract version info
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

# function to write versioned rds files
versioned_write_rds <- function(data, vi){
  
  # note; vi should come from versioning_proc()
  
  # checking if packages are installed
  pkgs <- c("glue", "readr")
  for (i in pkgs) {
    if (!requireNamespace(i, quietly = TRUE)) {
      stop(paste0("Package ", i," is required but not installed."))
    }
  }
  
  # package loading
  lapply(pkgs, require, character.only = TRUE)
  
  # filepath/names
  f_current <- glue("output/{vi$script}-{vi$version}.rds")
  f_archive <- glue("output/archive/{vi$script}-{vi$version}-{Sys.Date()}.rds")
  
  # writes out
  write_rds(x = data, file = f_current) # current file based on version
  write_rds(x = data, file = f_archive) # archived version
  
}

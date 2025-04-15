# extract_ppt.R
# Matt Kmiecik
# Started 14 April 2025

# Purpose: preprocess the PPT data from raw Wagner Alometer files
# Input is a file path to PPT file

# for testing purposes
# file <- "data/PPT_only/Baseline/140/EH17-338-140-0edited.txt"

extract_ppt_log <- function(file = NA, ...){
  
  library(tidyverse)
  
  # extracting participant number (will be in file path)
  p <- "EH17-\\d*-\\d*-\\d*"
  ss <- str_extract(file, pattern = p) %>% # extracts subject number
    trimws() %>% # trims any white space
    gsub(" ", "", .) # removes any spaces
  
  # reading in table of values
  dd <- 
    suppressMessages(
      read_delim(
        file = file, 
        delim = "\t", 
        skip = 9, 
        trim_ws = TRUE, 
        col_names = FALSE,
        show_col_types = FALSE
      )
    )
  
  
  ## columns of the log file
  tcols <- 
    c(
      "date", "time", "time2", "test", "trial", "site", "peak_strain", 
      "peak_time", "rate", "end_strain", "total_time", "user"
    )
  
  # cleans up
  dd2 <- 
    dd %>% 
    separate(X1, sep = "\\s+", into = tcols) %>%
    unite("datetime", date, time, time2, sep = " ") %>%
    # preserves date time
    mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %I:%M:%S %p")) %>%
    mutate(across(.cols = test:total_time, .fns = ~as.numeric(.x))) %>%
    mutate(subject_id = ss, .before = trial)
  
  if (nrow(dd2) > 8) {
    message("Probably not a true Wagner log file...")
    dd2 <- NA
  } else{
    return(dd2) # returns dataframe to user 
  }
  
}








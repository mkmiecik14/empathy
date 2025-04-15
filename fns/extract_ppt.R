# extract_ppt_log.R
# Matt Kmiecik
# Started 14 April 2025

# Purpose: preprocess the PPT data from raw Wagner Alometer files that are logs
# Input is a file path to PPT log file

# for testing purposes
# file <- "data/PPT_only/Baseline/140/EH17-338-140-0edited.txt"

extract_ppt <- function(file = NA, ...){
  
  library(tidyverse)
  
  # extracting date and time
  lines <- readLines(file)
  date_pattern <- "\\b\\d{2}-\\d{2}-\\d{4}\\b" # regexpr for date MM-DD-YYYY
  matched_lines <- grep(date_pattern, lines, value = TRUE) # finds date
  datetime <- as.POSIXct(matched_lines, format = "%m-%d-%Y %H:%M:%S") # converts
  
  # extracting participant number
  p <- "Subject ID" # regexpr for subject id
  ss <- 
    grep(p, lines, value = TRUE) %>% # grabs line with Subject ID
    str_extract(pattern = "EH17.*") %>% # extracts subject number
    gsub(" ", "", .) # removes any spaces
  
  # reading in table of values
  dd <- 
    suppressMessages(
      read_delim(
        file = file, 
        delim = "         ", 
        skip = 14, 
        trim_ws = TRUE, 
        col_names = FALSE,
        show_col_types = FALSE
      )
    )
  
  # cleans up
  dd2 <- 
    dd[,1:2] %>% 
    separate(2, sep = "\\s+", into = c("site", "strain", "rate", "duration")) %>%
    mutate(strain = gsub("\\*", "", strain)) %>%
    rename(trial = X1) %>%
    mutate(across(.cols = everything(), .fns = ~as.numeric(.x))) %>%
    mutate(subject_id = ss, datetime = datetime, .before = trial)
  
  return(dd2) # returns dataframe to user
  
}
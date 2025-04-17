# extract_ppt_log.R
# Matt Kmiecik
# Started 14 April 2025

# Purpose: preprocess the PPT data from raw Wagner Alometer files that are logs
# Input is a file path to PPT log file

# for testing purposes
 file <- "data/PPT_only/Baseline/24/EH17-338-24-0-on03-28-2019at12-41-37.txt"
#file <- "data/PPT_only/Baseline/12/EH17-338-12-0-on03-04-2019at18-38-53.txt"

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
    str_extract(pattern = "EH17-338-\\d{1,3}-\\d{1}") %>% # extracts subject number
    gsub(" ", "", .) # removes any spaces
  
  # if subject number is missing grab it from the file path
  if (is.na(ss)) {
    # this is a WagnerAlgometerLog file
    # checks if file path has "Baseline", "Period Visit 1" , "Period Visit 2"
    visit <- 
      case_when(
        str_detect(file, "Baseline") ~ 0,
        str_detect(file, "Period Visit 1") ~ 1,
        str_detect(file, "Period Visit 2") ~ 2,
        .default = NA
      )
    
    # retrives subject number
    ss_num <- 
      str_extract(file, pattern = "/\\d{1,3}(?=-|/|\\b)") %>% gsub("/", "", .)
    
    # assembles full ss number that matches
    ss <- paste0("EH17-338-", ss_num, "-", visit)
    
  } else{} # nothing else needs to be done
  
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
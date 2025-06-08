# prepro-long-pain.R
# Matt Kmiecik
# Started 07 June 2025

# Purpose: preprocess the longitudinal pain ratings

# libraries ----
library(tidyverse); library(glue); library(here)

# For versioning ----
#args <- "v1" # when testing
args <- commandArgs(trailingOnly = TRUE)
version <- ifelse(length(args) == 0, stop("version not specified"), args)
output_dir <- here("output")
script <- "prepro-long-pain"

# data ----
f <- 
  file.path(output_dir, "EH17338EMPATHY_DATA_2024-11-07_1140_noPHI-joined.csv")
data <- read_csv(f)

# proc ----
pain_cols <- paste0("painrating_child_q", 1:4)
data_proc <- 
  data %>% 
  select(subject_id, redcap_event_name, todaysdate, all_of(pain_cols)) %>%
  filter(!is.na(subject_id)) %>%
  distinct() %>% # removes duplicate rows
  mutate(todaysdate = mdy(todaysdate)) %>%
  filter(!grepl("baseline", redcap_event_name)) %>% # removes baseline
  mutate(
    baselinedate = min(todaysdate), .by = subject_id, .after = todaysdate
    ) %>%
  mutate(days_since_baseline = todaysdate - baselinedate, .after = baselinedate)

# writes out ----
fname <- glue("{script}-{version}.rds") # dynamic file name
f <- file.path(output_dir, fname) # constructs file path
write_rds(data_proc, file = f) # writes file
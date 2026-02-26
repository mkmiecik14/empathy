# prepro-aud-acuity.R
# Matt Kmiecik
# Started 26 Dec 2024

# Purpose: to preprocess the auditory acuity E-Prime task data

# libraries ----
library(tidyverse)

# data ----

# file name
f <- "data/auditory-acuity-eprime/auditory-acuity-eprime-merge-2024-12-26.txt"

# columns to extract
cc <- c("ExperimentName", "Subject", "Session", "SessionDate")

# processing pipeline
dd <- 
  read_delim(file = f, delim = "\t", guess_max = 2000) %>% # reads data
  select(all_of(cc)) %>% # selects only relevant cols
  mutate(date = as.Date(SessionDate, format = "%m-%d-%Y")) %>%
  select(task = ExperimentName, ss = Subject, session = Session, date) %>%
  distinct() %>%
  arrange(ss, date)

# query 1: general session info
f <- file.path("output", "aud-acuity-query-1.csv")
write_csv(dd, file = f)

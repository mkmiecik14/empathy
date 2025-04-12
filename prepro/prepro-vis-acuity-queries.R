# prepro-vis-acuity.R
# Matt Kmiecik
# Started 26 Dec 2024

# Purpose: to preprocess the visual acuity E-Prime task data

# libraries ----
library(tidyverse)

# data ----

# file name
f <- "data/visual-acuity-eprime/visual-acuity-eprime-merge-2024-12-26.txt"

# columns to extract
cc <- 
  c(
    "ExperimentName", "Subject", "Session", "SessionDate", 
    paste0("Line", 1:8, ".RESP")
    )

# processing pipeline
dd <- 
  read_delim(file = f, delim = "\t", guess_max = 2000) %>% # reads data
  select(all_of(cc)) %>% # selects only relevant cols
  mutate(date = as.Date(SessionDate, format = "%m-%d-%Y")) %>%
  select(
    task = ExperimentName, ss = Subject, session = Session, date, 
    ends_with(".RESP")
    ) %>%
  pivot_longer(
    cols = c(-task, -ss, -session, -date), names_to = "trial", values_to = "acc"
    ) %>%
  mutate(
    trial = regmatches(trial, regexpr("\\d", trial)),
    trial = as.numeric(trial),
    acc = case_when(acc == "Y" ~ 1, acc == "N" ~ 0, .default = NA)
    )

# Here to fix session or other issues
#dd %>% select(task, ss, session, date) %>% distinct() %>% arrange(ss, session) %>% View()

dd_f <- 
  dd %>% 
  mutate(
    session = case_when(
      # ss 90 session 10 on 2020-07-02 was supposed to be session 11:
      ss == 90 & date == as.Date("2020-07-02") ~ 11, 
      .default = session
      )
    )

dd_f_d <- 
  dd_f %>% 
  select(task, ss, session, date) %>% 
  distinct() %>% 
  arrange(ss, session)

# saving out for questions to staff ----
f <- file.path("output", "visual-acuity-data-queries.csv")
write_csv(dd_f_d, file = f)


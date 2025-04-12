# redcap_join.R
# Matt Kmiecik
# Started 11 April 2025

# Purpose: join the visual task, auditory task, and visual acuity task data
# with the data exported from redcap

# libraries ----
library(tidyverse)


# data ----

## redcap data
d <- "EH17338EMPATHY_DATA_2024-11-07_1140_noPHI.csv"
f <- file.path("data", d)
rdcp <- read_csv(f)

## task data
f <- list.files(path = "output", pattern = "sum-data.rds")
tmp <- vector("list", length = length(f))
names(tmp) <- gsub("-", "_", f)               # Replace hyphens
names(tmp) <- gsub("\\.rds$", "", names(tmp)) # Remove .rds extension

# reads in data
for (i in 1:length(tmp)) {
  tmp[[i]] <- readRDS(file = file.path("output", f[i]))
}

rdcp %>% filter(redcap_event_name == "annual_year_1_arm_1") %>% select(where(~ !all(is.na(.)))) %>% View()
rdcp %>% 
  select(record_id, subject_id, redcap_event_name, contains("date")) %>% 
  filter(!is.na(todaysdate)) %>%
  relocate(todaysdate, .after = redcap_event_name)
  View()


# combines all task data
task_data <- 
  reduce(tmp, full_join, by = c("ss", "session", "date")) %>% 
  arrange(ss, session) %>%
  # adds redcap data to help with join
  mutate(
    redcap_event_name = case_when(
      session == 10 ~ "baseline_visit_chi_arm_1",
      session == 11 ~ "visit_1_child_arm_1",
      session == 12 ~ "visit2_child_arm_1",
      .default = "missing"
    ),
    .after = date
    )

# joins with redcap
comb <- 
  rdcp %>%
  left_join(., task_data, by = c("subject_id" = "ss", "redcap_event_name"))

# writes out data ----

## csv
d2 <- gsub("\\.csv$", "", d) # removes .csv from filename
f <- file.path("output", paste0(d2, "-joined", ".csv"))
write_csv(comb, file = f)

## rds
f <- file.path("output", "redcap-data-joined.rds")
saveRDS(comb, file = f)


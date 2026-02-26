# prepro-bladder-task.R
# Matt Kmiecik
# Started 2025-05-13

# Purpose: preprocess the after pain ratings for analysis

# libraries ----
library(tidyverse); library(hms)

# data ----
f <- file.path("output", "EH17338EMPATHY_DATA_2024-11-07_1140_noPHI-joined.csv")
dd <- read_csv(f)

f <- file.path("doc", "bladder-test-dict.xlsx")
dict <- readxl::read_excel(f)

# proc ----

# selecting data
header <- c("record_id", "redcap_event_name", "subject_id")
events <- 
  c("baseline_visit_chi_arm_1", "visit_1_child_arm_1", "visit2_child_arm_1")
dd2 <- 
  dd %>% 
  select(all_of(header), starts_with("bt")) %>%
  filter(redcap_event_name %in% events, !is.na(subject_id))

# examining pain ratings over time

# lines up pain, time, and urgency ratings
pain <- 
  dd2 %>% 
  select(subject_id, redcap_event_name, matches("pain")) %>%
  pivot_longer(
    cols = c(-subject_id, -redcap_event_name), 
    names_to = "bt_pain", 
    values_to = "pain"
    ) %>%
  distinct() %>% # takes care of repeated data
  left_join(., dict %>% select(bt_pain, shared), by = "bt_pain")

time <- 
  dd2 %>% 
  select(subject_id, redcap_event_name, matches("time")) %>%
  pivot_longer(
    cols = c(-subject_id, -redcap_event_name),
    names_to = "bt_time",
    values_to = "time"
    ) %>%
  distinct() %>% # takes care of repeated data
  left_join(., dict %>% select(bt_time, shared), by = "bt_time")

urg <-  
  dd2 %>% 
  select(subject_id, redcap_event_name, matches("urg")) %>%
  pivot_longer(
    cols = c(-subject_id, -redcap_event_name),
    names_to = "bt_urgency",
    values_to = "urgency"
  ) %>%
  distinct() %>% # takes care of repeated data
  left_join(., dict %>% select(bt_urgency, shared), by = "bt_urgency")
  
# combines data and calculates the timing in minutes
bladder_task <- 
  full_join(pain, time, by = c("subject_id", "redcap_event_name", "shared")) %>%
  full_join(., urg, by = c("subject_id", "redcap_event_name", "shared")) %>%
  select(
    subject_id, redcap_event_name, bt_time, bt_urgency, bt_pain, shared, time, 
    urgency, pain
    ) %>%
  mutate(
    min_time = min(time, na.rm = TRUE), .by = c(subject_id, redcap_event_name)
    ) %>%
  mutate(diff_time = as.numeric(time - min_time, units = "mins"))

# writes out data ----
f <- file.path("output", "bladder-task-long-data.rds")
write_rds(bladder_task, file = f)

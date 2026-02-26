# prepro-afterpain.R
# Matt Kmiecik
# Started 2025-05-13

# Purpose: preprocess the after pain ratings for analysis

# libraries ----
library(tidyverse)

# data ----
f <- file.path("output", "EH17338EMPATHY_DATA_2024-11-07_1140_noPHI-joined.csv")
dd <- read_csv(f)

# proc ----

# selecting data
header <- c("record_id", "redcap_event_name", "subject_id")
vars <- 
  c(
    "pt3a1", "pt3b1", "pt3a2", "ptb2", "pt4b_knee", "pt4c_shoulder", 
    "postcpm_hand"
    )
events <- 
  c("baseline_visit_chi_arm_1", "visit_1_child_arm_1", "visit2_child_arm_1")
dd2 <- 
  dd %>% 
  select(all_of(header), all_of(vars)) %>%
  filter(redcap_event_name %in% events)

# cleaning up
res <- 
  dd2 %>%
  select(
    subject_id, redcap_event_name, 
    afterpain_knee_1 = pt3a1,
    afterpain_shoulder_1 = pt3b1,
    afterpain_knee_2 = pt3a2,
    afterpain_shoulder_2 = ptb2,
    afterpain_knee_3 = pt4b_knee,
    afterpain_shoulder_3 = pt4c_shoulder,
    afterpain_hand = postcpm_hand
    )

# writing out ----
f <- file.path("output", "afterpain-data.rds")
write_rds(res, file = f)

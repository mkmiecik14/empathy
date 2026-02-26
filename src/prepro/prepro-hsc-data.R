# prepro-hsc-data.R
# Matt Kmiecik
# 2025-05-25

# Purpose: preprocess the data for the highly sensitive child questionnaire

# libraries ----
library(tidyverse)

# data ----

## redcap data
d <- "EH17338EMPATHY_DATA_2024-11-07_1140_noPHI.csv"
f <- file.path("data", d)
rdcp <- read_csv(f)

# procedure ----
rdcp_events <- 
  c("baseline_visit_chi_arm_1", "visit_1_child_arm_1", "visit2_child_arm_1" )
hsc <- 
  rdcp %>% 
  select(subject_id, redcap_event_name, starts_with("hscq")) %>%
  filter(
    !is.na(subject_id), 
    redcap_event_name %in% rdcp_events
    ) %>%
  pivot_longer(cols = c(-subject_id, -redcap_event_name)) %>%
  summarise(
    hsc_mean = mean(value), n = n(), 
    .by = c(subject_id, redcap_event_name)
    )

# quick checks that these data make sense (uncomment to see)
# hsc %>% filter(n != 12) # no one has any more or less than 12 questions
# ggplot(hsc, aes(hsc_mean, color = redcap_event_name)) + geom_density()

# writes out ----
hsc[["n"]] <- NULL # removes the n column after checking
f <- file.path("output", "hsc-data.rds")
write_rds(hsc, file = f)

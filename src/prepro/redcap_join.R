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

## visual/ auditory task data
f <- list.files(path = "output", pattern = "sum-data.rds")
tmp <- vector("list", length = length(f))
names(tmp) <- gsub("-", "_", f)               # Replace hyphens
names(tmp) <- gsub("\\.rds$", "", names(tmp)) # Remove .rds extension

# reads in data
for (i in 1:length(tmp)) {
  tmp[[i]] <- readRDS(file = file.path("output", f[i]))
}

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

## PPT data
f <- file.path("output", "ppt-data.rds")
ppt_data <- readRDS(file = f)

# summarises PPT by subject
ppt_ss <- 
  ppt_data %>%
  filter(!is.na(strain)) %>% # removes missing data
  summarise(m = mean(strain), n = n(), .by = c(ss, session, location, task)) %>%
  # adds redcap data to help with join
  mutate(
    redcap_event_name = case_when(
      session == 0 ~ "baseline_visit_chi_arm_1",
      session == 1 ~ "visit_1_child_arm_1",
      session == 2 ~ "visit2_child_arm_1",
      .default = "missing"
    )
  )

ppt_ss_wide <- 
  ppt_ss %>%
  pivot_wider(
    id_cols = c(ss, session, redcap_event_name), 
    values_from = m, 
    names_from = c(location, task)
    ) %>%
  # calculates effect of CPM
  mutate(
    knee_CPM_sub = knee_CPM - knee_PPT, 
    shoulder_CPM_sub = shoulder_CPM - shoulder_PPT
    )

# examining the relationship between knee and shoulder CPM
# uncomment to see
# tmp <- ppt_ss_wide %>% filter(session == 0)
# ggplot(tmp, aes(shoulder_CPM_sub, knee_CPM_sub)) + geom_point() + geom_smooth(method = "lm")
# cor.test(tmp$knee_CPM_sub, tmp$shoulder_CPM_sub)
#
# I am deciding not to z-score and combine the CPM measures. This is because:
# 1. attrition over time may not be random (dist will change)
# 2. shoulder and knee are correlated, but modestly

# prepping data for join ----

## auditory / visual task data
task_data_j <- task_data %>% select(-session, -date)

## PPT / CPM data
ppt_data_j <- 
  ppt_ss_wide %>% 
  select(
    ss, redcap_event_name, knee_PPT, shoulder_PPT, 
    knee_CPM = knee_CPM_sub, shoulder_CPM = shoulder_CPM_sub
    )

# joins with redcap
comb <- 
  rdcp %>%
  left_join(., task_data_j, by = c("subject_id" = "ss", "redcap_event_name")) %>%
  left_join(., ppt_data_j, by = c("subject_id" = "ss", "redcap_event_name"))

# writes out data ----

## csv
d2 <- gsub("\\.csv$", "", d) # removes .csv from filename
f <- file.path("output", paste0(d2, "-joined", ".csv"))
write_csv(comb, file = f)

## rds
f <- file.path("output", "redcap-data-joined.rds")
saveRDS(comb, file = f)


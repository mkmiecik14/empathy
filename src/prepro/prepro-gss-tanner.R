# prepro-gss-tanner.R
# Matt Kmiecik
# Started 12 May 2025

# Purpose: cleans and preps the GSS and Tanner stage data across all visits

# libraries ----
library(tidyverse); library(readxl)

# data ----

## gets file name and path
f <- "Tanner_Body Map_Fibromyalgia Data_ES_8.19.24_NOfixed.xlsx" # file
fpath <- file.path("data", f)

## preallocates info about to be read in
sheets <- c("Baseline", "PV1", "PV2")
tmp <- as.list(sheets)
names(tmp) <- sheets

# reads in and combines all visits
dd <- 
  tmp %>% 
  map(~read_excel(path = fpath, sheet = .x, na = c("NA", "N/A", ""))) %>% 
  map(~.x[-1,]) %>% 
  list_rbind(names_to = "visit")

# cleaning
body_vars <- paste0("Body_", 1:7)
dd_wide <- 
  dd %>% 
  select(
    subject_id, visit, tanner_breast = breast, tanner_hair = hair, all_of(body_vars), 
    q_drymouth = DryMouth, q_rapidhr = RapidHR, q_balance = Balance, 
    q_chemical = `Chemical sensitivity`, q_sound = `Sound sensitvity`,
    q_light = `Light sensitivity`
    ) %>%
  mutate(across(.cols = tanner_breast:q_light, .fns = ~as.numeric(.x))) 

# long format to compute totals
dd_long_calc <- 
  dd_wide %>% 
  select(-starts_with("tanner")) %>%
  pivot_longer(cols = c(-subject_id, -visit)) %>%
  separate(name, into = c("section", "q")) %>%
  summarise(s = sum(value), n = n(), .by = c(subject_id, visit, section))

# checks validity (uncomment to see )
# dd_long_calc %>% filter(section == "Body", n !=7)
# dd_long_calc %>% filter(section == "Body", s > 7)
# dd_long_calc %>% filter(section == "Body", s < 0)
# dd_long_calc %>% filter(section == "q", n != 6)
# dd_long_calc %>% filter(section == "q", s > 6)
# dd_long_calc %>% filter(section == "q", s < 0)

# converts back to wide format and tidys up variables
data <- 
  dd_long_calc %>% 
  pivot_wider(
    id_cols = c(subject_id, visit), names_from = section, values_from = s
    ) %>%
  # joins in tanner staging data
  left_join(
    ., 
    dd_wide %>% select(subject_id, visit, starts_with("tanner")), 
    by = c("subject_id", "visit")
    ) %>%
  rename(redcap_event_name = visit, bodymap = Body, gss = q) %>%
  mutate(
    redcap_event_name = case_when(
      redcap_event_name == "Baseline" ~ "baseline_visit_chi_arm_1",
      redcap_event_name == "PV1" ~ "visit_1_child_arm_1",
      redcap_event_name == "PV2" ~ "visit2_child_arm_1",
      .default = redcap_event_name
      )
    )

# writing out ----
f <- file.path("output", paste0("tanner-body-gss-data.rds"))
write_rds(data, f)

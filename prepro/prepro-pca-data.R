# prepro-pca-data.R
# Matt Kmiecik
# Started 27 April 2025

# Purpose: prepare sensory measures and questionnaires for 
# principal component analysis (PCA)

# libraries ----
library(tidyverse)

# data ----
f <- file.path("output", "EH17338EMPATHY_DATA_2024-11-07_1140_noPHI-joined.csv")
dd <- read_csv(f)

# re-used variables ----
header <- c("record_id", "redcap_event_name", "subject_id")

## duplicate entries in main df  
ss <- 
  dd %>% 
  select(all_of(header)) %>% 
  filter(!is.na(subject_id)) %>% 
  count(subject_id, redcap_event_name) %>% 
  filter(n!=1)

# proof that these are true duplicate entries and can be deleted
dd %>% 
  semi_join(ss, by = c("subject_id", "redcap_event_name")) %>%
  select(where(~ all(!is.na(.x)))) %>%
  summarise(
    all_identical = n_distinct(across(everything())) == 1, 
    n = n(),
    .by = c(subject_id, redcap_event_name)
    )

# removes duplictated participant entries here:
df <- 
  dd %>% 
  filter(!is.na(subject_id)) %>%
  distinct(subject_id, redcap_event_name, .keep_all = TRUE)

# Childhood Somatization Inventory ----
cssi_long <- 
  df %>% 
  select(all_of(header), matches("^csi_child")) %>%
  pivot_longer(
    cols = c(-record_id, -redcap_event_name, -subject_id),
    names_to = "q", values_to = "value"
    )

# computes cssi by summing all 24 trials
cssi_long_sum <- 
  cssi_long %>%
  summarise(cssi = sum(value), n = n(), .by = c(subject_id, redcap_event_name))
#cssi_long_sum %>% filter(n!=24) # proof now that all subjects have 24 trials


# Michigan Body Map ----
dd %>% select(all_of(header), matches("v_kids_fibro_bodymap"))

# auditory and visual provoked unpleasantness ratings ----
vis_aud_data <- 
  df %>% 
  select(all_of(header), vis_mean, vis_slope, aud_mean, aud_slope) %>%
  select(
    subject_id, redcap_event_name, vis_mean, vis_slope, aud_mean, aud_slope
    )

# PPTs ----
ppt_data <- 
  df %>% 
  select(all_of(header), knee_PPT, shoulder_PPT, knee_CPM, shoulder_CPM) %>%
  select(
    subject_id, redcap_event_name, 
    knee_PPT, shoulder_PPT, knee_CPM, shoulder_CPM
  )


# bladder experimental pain testing ----
test <- 
  df %>% 
  select(all_of(header), matches("bladder"))


# combining data ----
tjoin <- c("subject_id", "redcap_event_name") # joining criteria
dc <- 
  full_join(cssi_long_sum %>% select(-n), vis_aud_data, by = tjoin) %>%
  full_join(., ppt_data, by = tjoin)

# writing out data ----
f <- file.path("output", "pca-data.rds")
write_rds(dc, file = f)

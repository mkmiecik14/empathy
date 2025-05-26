# prepro-cssi-data.R
# Matt Kmiecik
# Started 25 May 2025

# Purpose: prepare CSSI data

# libraries ----
library(tidyverse)

# data ----
f <- file.path("output", "EH17338EMPATHY_DATA_2024-11-07_1140_noPHI-joined.csv")
dd <- read_csv(f)

# re-used variables ----
header <- c("record_id", "redcap_event_name", "subject_id")

# removes duplicate entries ----

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

# removes duplicated participant entries here:
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

# removes n column that was used to check summation
res <- cssi_long_sum %>% select(-n)

# writes out data ----
f <- file.path("output", "cssi-data.rds")
write_rds(res, file = f)

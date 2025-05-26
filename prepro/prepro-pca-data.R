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

# removes duplicated participant entries here:
df <- 
  dd %>% 
  filter(!is.na(subject_id)) %>%
  distinct(subject_id, redcap_event_name, .keep_all = TRUE)

################
#              #
# EXPERIMENTAL #
#              #
################

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

# after pain data ----
f <- file.path("output", "afterpain-data.rds")
ap_data <- read_rds(file = f)


# bladder experimental pain testing (IMPUTED DATA) ----
f <- file.path("output", "bladder-task-imp-data.rds")
tmp <- read_rds(file = f) # reads in

# converts bladder task pain data from long to wide format
bt_pain_wide <- 
  tmp %>% 
  pivot_wider(
    id_cols = c(subject_id, redcap_event_name), 
    names_from = bt_pain, 
    values_from = pain
    )

# converts bladder task urgency data from long to wide format
bt_urgency_wide <- 
  tmp %>% 
  pivot_wider(
    id_cols = c(subject_id, redcap_event_name), 
    names_from = bt_urgency, 
    values_from = urgency
  )


# cold pain data ----
f <- file.path("output", "coldpain-data.rds")
cp_data <- read_rds(file = f) %>% select(-water_temp)

##################
#                #
# QUESTIONNAIRES #
#                #
##################

# Childhood Somatization Inventory ----
f <- file.path("output", "cssi-data.rds")
cssi_data <- read_rds(file = f)


# Michigan Body Map ----
f <- file.path("output", "tanner-body-gss-data.rds")
tmp <- readRDS(f)
bodymap_gss_data <- tmp %>% select(-starts_with("tanner")) # removes tanner data


# Highly sensitive child ----
f <- file.path("output", "hsc-data.rds")
hsc_data <- read_rds(file = f)

#################
#               #
# COMBINES DATA #
#               #
#################

# combining data ----
tjoin <- c("subject_id", "redcap_event_name") # joining criteria
all_ss <- df %>% select(subject_id, redcap_event_name) # all possible subjects

# creates a list of dataframes
df_list <- 
  list(
    all_ss, # participant numbers and events
    # experimental:
    vis_aud_data, ppt_data, bt_pain_wide, bt_urgency_wide, cp_data, ap_data,
    cssi_data, bodymap_gss_data, hsc_data # questionnaires
    )

# combines all data into one dataframe
pca_data <- reduce(df_list, full_join, by = tjoin)


# writing out data ----
f <- file.path("output", "pca-data.rds")
write_rds(pca_data, file = f)

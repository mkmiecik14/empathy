# prepro-ppt.R
# Matt Kmiecik
# Started 14 April 2025

# Purpose: preprocess the PPT data from raw Wagner Alometer files

# libraries ----
library(tidyverse)

# functions ----
source("src/fns/extract_ppt.R") # for processing regular ppt files
source("src/fns/extract_ppt_log.R") # for processing log files

# proc ----

## determines which bsae filepaths to cycle through
base_dir <- 
  paste0("data/PPT_only/", c("Baseline", "Period Visit 1", "Period Visit 2"))

## all ppt file paths
dir_list <- as.list(base_dir)
ppt_files<- 
  dir_list %>% 
  map(
    ~list.files(.x, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
    ) %>%
  unlist()

## preallocations
tlen <- length(ppt_files) # change this to an integer for testing
tmp1 <- vector("list", length = tlen) -> tmp2 # preallocates lists
names(tmp1) <- ppt_files[1:tlen] # names lists by file path
names(tmp2) <- ppt_files[1:tlen] # names lists by file path

# loops through files and processes them
pb <- txtProgressBar(min = 0, max = length(tmp1), style = 3) # prog bar
for (i in 1:length(tmp1)) {
  
  # tmp1 stores the regular ppt files
  tmp1[[i]] <- 
    tryCatch(
      {
        extract_ppt(file = ppt_files[i]) # attempts a pull on ppt data
      },
      error = function(e){
        #message(sprintf("Error on i = %d: %s", i, e$message))
        return(NA)  # or NULL or whatever fallback makes sense
      }
    )
  
  # tmp2 stores the ppt log files
  tmp2[[i]] <- 
    tryCatch(
      {
        extract_ppt_log(file = ppt_files[i]) # attempts a pull on ppt data
      },
      error = function(e){
        #message(sprintf("Error on i = %d: %s", i, e$message))
        return(NA)  # or NULL or whatever fallback makes sense
      }
    )
  
  # Update progress bar
  setTxtProgressBar(pb, i)
  
}; close(pb)


# consolidates ppt files:
ppt_res <- Filter(is.data.frame, tmp1) %>% list_rbind(names_to = "path")
ppt_log_res <- Filter(is.data.frame, tmp2) %>% list_rbind(names_to = "path")

# grabbing all participant IDs and date times if they exist
ss_key <- 
  ppt_res %>% 
  select(subject_id, datetime) %>%
  bind_rows(., ppt_log_res %>% select(subject_id, datetime)) %>%
  mutate(date = as.Date(datetime)) %>%
  select(-datetime) %>%
  distinct() %>%
  separate(
    subject_id, into = c("study", "study_id", "ss", "session"), sep = "-",
    remove = FALSE
  ) %>%
  mutate(across(.cols = ss:session, .fns = ~as.numeric(.x))) %>%
  arrange(ss, session)

# subjects with less or more than 6 trials, indicating not a clean run
ss <- ppt_res %>% count(subject_id) %>% filter(n!=6) %>% pull(subject_id)

# data that is likely clean
ppt_data <- ppt_res %>% filter(!subject_id %in% ss)

pj <- position_jitter(width = .2)
ggplot(ppt_data, aes(factor(trial), strain)) +
  geom_point(shape = 19, alpha = 1/3, position = pj) +
  geom_boxplot(
    aes(group = trial), position = position_nudge(x = .4), width = .2
    ) +
  theme_bw()

# taking a look at subjects with multiple files/runs/issues
# ppt_res %>% filter(subject_id %in% ss) %>% View()
ppt_res_2 <- 
  ppt_res %>% 
  filter(subject_id %in% ss) %>% # includes only problem ss
  mutate(
    edited_fixed = grepl("fixed|edited", path, ignore.case = TRUE), 
    .before = subject_id
    )

# pulls subject numbers that are still issues after using edited or fixed
ss_2 <- 
  ppt_res_2 %>% 
  filter(edited_fixed) %>% 
  count(subject_id) %>% 
  filter(n!=6) %>% 
  pull(subject_id)

# after taking a look, these participants will be OK to use as long as we
# use the edited_fixed==TRUE flag
#ppt_res_2 %>% filter(subject_id %in% ss_2) %>% View()

# Therefore:
ppt_data_2 <- 
  bind_rows(
    ppt_data, 
    ppt_res_2 %>% filter(edited_fixed) %>% select(-edited_fixed)
    )

# ppt_data_2 should contain a lot of solid data; now to see how much is missing
# and to grab whatever is missing from the ppt_log_res (i.e., Wagner log files)

## taking a quick look to see how much is missing
rr <- 
  ppt_data_2 %>% 
  count(subject_id) %>%
  left_join(ss_key, ., by = "subject_id")
rr %>% count(n) # wow, only 26!
ss_3 <- rr %>% filter(is.na(n)) %>% pull(subject_id) # subjects still missing

# participants in wagner and their trial counts
log_n <- 
  ppt_log_res %>% 
  filter(subject_id %in% ss_3) %>%
  count(subject_id)

ss_4 <- log_n %>% filter(n==6) %>% pull(subject_id) # good ss from log files
log_data_1 <- ppt_log_res %>% filter(subject_id %in% ss_4) # GOOD LOG DATA

ss_5 <- log_n %>% filter(n!=6) %>% pull(subject_id) # problem ss from log files

# brings in query results
ppt_query_ans <- 
  readxl::read_excel(path = file.path("doc", "ppt-query_SD.xlsx")) %>%
  mutate(datetime = ymd_hms(datetime, tz = "UTC")) %>%
  # this extra step pushes time ahead 7 hours to match (bc of Excel)
  mutate(datetime = with_tz(datetime, tzone = "America/Los_Angeles"))

# preps for join
bb <- c("path", "datetime", "subject_id", "trial", "site")
ppt_query_join <- 
  ppt_query_ans %>% 
  select(all_of(bb), query_trial)

# CLEANED REMAINING LOG DATA
log_data_2 <- 
  ppt_log_res %>% 
  filter(subject_id %in% ss_5) %>%
  left_join(., ppt_query_join, by = bb) %>%
  filter(!is.na(query_trial))

# combines log data
log_data <- 
  bind_rows(log_data_1, log_data_2) %>%
  # does some selecting and renaming of columns to facilitate join
  select(
    path, subject_id, datetime, trial, site, strain = peak_strain, rate, 
    duration = total_time
    )

# combines ppt and log data into one
ppt_data_all <- bind_rows(ppt_data_2, log_data)

# checking data that are still missing
ss_6 <- 
  ppt_data_all %>% 
  count(subject_id) %>%
  left_join(ss_key, ., by = "subject_id") %>%
  filter(is.na(n))
# these participants are still OK, they just had identical ppt and wagner log
# files, resulting in being skipped over
ppt_data_3 <- 
  ppt_res %>% 
  filter(subject_id %in% ss_6$subject_id) %>% 
  filter(!grepl("Wagner", path)) # excludes wagner log data

# combines remaining ppt data with all data and does some organizing
ppt_data_all_2 <- 
  bind_rows(ppt_data_all, ppt_data_3) %>% # combines
  separate(
    subject_id, into = c("study", "study_id", "ss", "session"), sep = "-",
    remove = FALSE
  ) %>%
  mutate(
    location = case_when(
      site == 3 ~ "knee", 
      site == 1 ~ "shoulder",
      is.na(site) ~ NA_character_,
      .default = as.character(site)
      ),
    task = case_when(
      trial %in% c(1:4) ~ "PPT",
      trial %in% c(5:6) ~ "CPM",
      is.na(trial) ~ NA_character_,
      .default = as.character(trial)
    ),
    .before = strain
    ) %>%
  select(-study, -study_id) %>% # removes uninformative cols
  mutate(across(.cols = c(ss, session), .fns = ~as.numeric(.x)))
  

# These are data in which n trials is not 6
# comment out to see
# ppt_data_all_2 %>%
#   filter(!is.na(strain)) %>%
#   count(subject_id) %>%
#   left_join(ss_key, ., by = "subject_id") %>%
#   filter(n != 6)

# saves out PPT data ----

## RDS
f <- file.path("output", "ppt-data.rds")
saveRDS(ppt_data_all_2, file = f)

## CSV
f <- file.path("output", "ppt-data.csv")
write_csv(ppt_data_all_2, file = f)


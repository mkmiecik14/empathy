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
ppt_res %>% filter(subject_id %in% ss) %>% View()
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
ppt_log_res %>% filter(subject_id %in% ss_5) %>% View()
# One can be saved by using the "EDITED" log file, the rest are likely not great

ppt_query <- ppt_log_res %>% filter(subject_id %in% ss_5)
write_csv(ppt_query, file = "output/ppt-query.csv")

# combines ppt and log data into one
ppt_data_all <- 
  log_data_1 %>% 
  select(
    path, subject_id, datetime, trial, site, strain = peak_strain, rate, 
    duration = total_time
    ) %>%
  bind_rows(ppt_data_2, .)


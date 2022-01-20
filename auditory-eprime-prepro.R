# EMPATHY Auditory Task E-Prime Preprocessing
# Matt Kmiecik
# Started 6 May 2021

# Purpose: to preprocess the auditory task e-data aid files from eprime

# Prepare R workspace ----
source("r-prep.R")

# Table to include db values later ----
db_conv <- tibble(stim = 1:5, db = c(30, 45, 60, 75, 90))

# Loads data ----
auditory_data <- 
  read_delim(
    file = "../data/eprime/auditory/empathy-auditory-merge-6-may-2021.txt", 
    delim = "\t", # tab delimited
    guess_max = 2000 # this is necessary due to parsing failures
  )

# Preprocesses and cleans data ----
doubles <- c(22, 33, 44, 55, 66, 77, 88, 99) # for correction done below
auditory_data_long <- 
  auditory_data %>%
  # selects relevant variables
  select(
    ss = Subject,
    session = Session,
    date = SessionDate,
    time = SessionTime,
    order = Block,
    stim = BlockList,
    rating_0 = RateSc.RESP,
    rating_1 = RateSc1.RESP,
    rating_2 = RateSc2.RESP,
    rating_3 = RateSc3.RESP,
    rating_4 = RateSc4.RESP,
  ) %>%
  mutate(across(starts_with("rating"), as.numeric)) %>% # changes ratings to num
  pivot_longer(
    cols = starts_with("rating"),
  ) %>%
  rename(rating = value) %>%
  filter(complete.cases(rating)) %>% # gets rid of missing values
  select(-name) %>% # gets rid of useless column
  left_join(., db_conv, by = "stim") %>%
  select(ss, session, date, time, stim, order, db, rating) %>% # org columns
  arrange(ss, session, stim) %>% # arranges for aesthetics

# Examining data quality
# ggplot(auditory_data_long, aes(rating)) +
#   geom_histogram(binwidth = 1)
# test <- auditory_data_long %>% filter(rating %nin% 0:20)
# write_csv(test, file = "../output/audio-dq.csv")

# Explanation from Gabby why ratings can exceed 20:
# We have to input two numbers to move to the next sound/image, 
# so if the participant rates the pain a 2 we input 02. However if we hear the 
# participant say "two" and then automatically press 2, we have to put another 
# number down for the task to continue. In these situations we always put the 
# same number twice because it doesn't exist as an answer so we recognize the 
# error for what it really is. I feel confident in saying you can operate under 
# the impression that "22" is a "2", "99" is a "9" and "55" is a "5".
# Therefore, these numbers will be replaced with the singular number version
  mutate(rating = ifelse(rating %in% doubles, rating %% 10, rating)) # here

# Test done here to prove that ratings > 20 are fixed
# auditory_data_long %>% count(rating) %>% View()

# Wide format for Excel users ----
auditory_data_wide <- 
  auditory_data_long %>% 
  select(ss, session, stim, rating) %>%
  pivot_wider(id_cols = c(ss, session), names_from = stim, values_from = rating)

# Saves out data ----
save(auditory_data_long, file = "../output/auditory-data-long.RData") # Rdata long
write_csv(auditory_data_long, file = "../output/auditory-data-long.csv") # csv long
write_csv(auditory_data_wide, file = "../output/auditory-data-wide.csv") # csv wide

# Cleans up script objects ----
rm(db_conv, auditory_data, auditory_data_long, auditory_data_wide, doubles)  
  
  
  
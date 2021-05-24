# EMPATHY Visual Task E-Prime Preprocessing
# Matt Kmiecik
# Started 6 May 2021

# Purpose: to preprocess the visual task e-data aid files from eprime

# Prepare R workspace ----
source("r-prep.R")

# Table to include lux values later ----
lux_conv <- tibble(stim = 1:5, lux = c(1, 30, 60, 90, 120))

# Loads data ----
visual_data <- 
  read_delim(
    file = "../data/eprime/visual/empathy-visual-merge-6-may-2021.txt", 
    delim = "\t", # tab delimited
    guess_max = 2000 # this is necessary due to parsing failures
  )

# Preprocesses and cleans them ----
visual_data_long <- 
  visual_data %>%
  select(
    ss = Subject, 
    session = Session, 
    date = SessionDate, 
    time = SessionTime,
    order = Block,
    stim = BlockList,
    stim1 = RateUnplblock1.RESP,
    stim2 = RateUnpl1block2.RESP,
    stim3 = RateUnplblock3.RESP,
    stim4 = RateUnplblock4.RESP,
    stim5 = RateUnplblock5.RESP,
  ) %>%
  mutate(date = as.Date(date, format = "%m-%d-%Y")) %>% # converts to date
  gather(intensity, rating, -ss, -session, -date, -time, -order, -stim) %>% # long format
  filter(complete.cases(rating)) %>% # removes "missing" data
  distinct(.) %>% # removes repeated rows
  select(-intensity) %>% # unneccesary column
  left_join(., lux_conv, by = "stim") %>% # includes lux column
  mutate(
    rating = gsub("{ENTER}", "", rating, fixed = TRUE), # wrong key was pressed in one entry
    rating = as.numeric(rating)
    ) %>%
  select(ss, session, date, time, stim, order, lux, rating) %>% # org columns
  arrange(ss, session, stim) # orders for better viewing

# Examining data quality
#ggplot(visual_data_long, aes(rating)) +
#  geom_histogram(binwidth = 1)
#visual_data_long %>% filter(rating %nin% 0:20)

# Wide format for Excel users ----
visual_data_wide <- 
  visual_data_long %>% 
  select(ss, session, stim, rating) %>%
  pivot_wider(id_cols = c(ss, session), names_from = stim, values_from = rating)

# Saves out data ----
save(visual_data_long, file = "../output/visual-data-long.RData") # Rdata long
write_csv(visual_data_long, file = "../output/visual-data-long.csv") # csv long
write_csv(visual_data_wide, file = "../output/visual-data-wide.csv") # csv wide

# Cleans up script objects ----
rm(lux_conv, visual_data, visual_data_long, visual_data_wide)

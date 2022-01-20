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
    file = "../data/eprime/visual/empathy-visual-merge-20-jan-2022.txt", 
    delim = "\t", # tab delimited
    guess_max = 2000 # this is necessary due to parsing failures
  )

# Preprocesses and cleans them ----
doubles <- c(22, 33, 44, 55, 66, 77, 88, 99) # for correction done below
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
  arrange(ss, session, stim) %>% # orders for better viewing

# Examining data quality
#ggplot(visual_data_long, aes(rating)) +
#  geom_histogram(binwidth = 1)
#visual_data_long %>% filter(rating %nin% 0:20)

# Explanation from Gabby why ratings can exceed 20:
# We have to input two numbers to move to the next sound/image, 
# so if the participant rates the pain a 2 we input 02. However if we hear the 
# participant say "two" and then automatically press 2, we have to put another 
# number down for the task to continue. In these situations we always put the 
# same number twice because it doesn't exist as an answer so we recognize the 
# error for what it really is. I feel confident in saying you can operate under 
# the impression that "22" is a "2", "99" is a "9" and "55" is a "5".
# Therefore, these numbers will be replaced with the singular number version
  mutate(rating = ifelse(rating %in% doubles, rating %% 10, rating)) %>% # here
# For the participant that has a rating of 87:
# Gabby ran this visit with Maggie...The participant was rating the visual tasks 
# at really high values (18-20) so we asked her if she would like to stop. 
# She said yes but we had one last visual image left before the task stopped so 
# we let it play without her watching it and then I put a random number 
# down (87) to end the task.
# Therefore, this number will be replaced with NA
  mutate(rating = ifelse(ss == 252 & rating == 87, NA, rating))

# proof that NA was added and ratings do not exceed 20
# visual_data_long %>% count(rating) %>% View()

# Visualization for data quality checks ----
# UNCOMMENT TO SEE
# Baselines
# this_data <- visual_data_long %>% filter(session == 10) # change to 11 or 12
# 
# # Violin plot
# pj <- position_jitter(width = .15, height = .1)
# ggplot(this_data, aes(factor(stim), rating)) +
#   geom_point(position = pj, alpha = 1/2) +
#   geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) + 
#   theme_minimal()
# 
# # linear
# pj <- position_jitter(width = .15, height = .1)
# ggplot(this_data, aes(stim, rating)) +
#   geom_point(position = pj, alpha = 1/2) +
#   geom_smooth(method = "lm", se = TRUE, color = "red") +
#   theme_minimal()

# Summary stats
visual_sum <- 
  visual_data_long %>% 
  filter(complete.cases(rating)) %>% # removes missing data points
  group_by(ss, session) %>%
  summarise(m = mean(rating), n = n()) %>%
  ungroup()

#visual_sum %>% filter(session>12)

# modeling slopes
visual_mods <-
  visual_data_long %>%
  nest_by(ss, session) %>%
  mutate(mod = list(lm(rating ~ 1 + scale(stim, scale = FALSE), data = data)))

visual_ests <-
  visual_mods %>%
  summarise(broom::tidy(mod)) %>%
  ungroup() %>%
  mutate(
    term = gsub("\\(Intercept\\)", "mean", term),
    term = gsub("scale\\(stim, scale = FALSE\\)", "slope", term)
    )

# proof that intercepts of mean centered reg models are identical to means
visual_sum %>% 
  left_join(., visual_ests %>% filter(term == "mean"), by = c("ss", "session"))

# Converting this to wide for excel users
visual_ests_wide <-
  visual_ests %>%
  pivot_wider(id_cols = c(ss, session), names_from = term, values_from = estimate)

# Wide format for Excel users ----
visual_data_wide <- 
  visual_data_long %>% 
  select(ss, session, stim, rating) %>%
  pivot_wider(id_cols = c(ss, session), names_from = stim, values_from = rating) %>%
  left_join(., visual_ests_wide, by = c("ss", "session"))

# Saves out data ----
save(visual_data_long, file = "../output/visual-data-long.RData") # Rdata long
write_csv(visual_data_long, file = "../output/visual-data-long.csv") # csv long
save(visual_data_wide, file = "../output/visual-data-wide.RData") # RData wide
write_csv(visual_data_wide, file = "../output/visual-data-wide.csv") # csv wide

# Cleans up script objects ----
rm(
  lux_conv, 
  visual_data, 
  visual_data_long, 
  visual_data_wide, 
  doubles,
  visual_ests,
  visual_ests_wide,
  visual_mods,
  visual_sum
  )

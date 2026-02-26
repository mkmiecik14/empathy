# analysis-visual-acuity-qc-1.R
# Matt Kmiecik
# 30 December 2024

# Purpose: perform quality checks on the visual acquity task

# libraries ----
library(tidyverse)

# data ----
dd <- readRDS("output/visual-acuity-data.rds")

# remove after these data have been cleaned
dc <- dd %>% filter(session %in% c(10:12)) 

# subject-wise summary
dc_sum <- 
  dc %>% 
  group_by(ss, session) %>% 
  summarise(m = mean(acc), n = n()) %>%
  ungroup()

# histogram
ggplot(dc_sum, aes(m)) + 
  geom_histogram(binwidth = .2) + 
  facet_wrap(~session)

# trial-wise summary
dc_trial <- 
  dc %>% 
  summarise(
    p = mean(acc), 
    n = n(), 
    se = sqrt((p * (1 - p)) / n), 
    .by = c(session, trial)
    ) 

# trial-wise accuracy
ggplot(dc_trial, aes(trial, p)) +
  geom_point() +
  geom_errorbar(aes(ymin = p-se, ymax = p+se), width = .1) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 8, 1), minor_breaks = NULL) + 
  facet_wrap(~session)

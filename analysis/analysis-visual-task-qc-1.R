# analysis-visual-task-qc-1.R
# Matt Kmiecik
# 30 December 2024

# Purpose: perform quality checks on the visual task

# libraries ----
library(tidyverse)

# data ----
dd <- readRDS(file = "output/vis-task-data.rds")

# remove after these data have been cleaned
dc <- dd %>% filter(session %in% c(10:12), rating %in% c(0:20)) 

# histogram of ratings by session
ggplot(dc, aes(rating)) + 
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  facet_wrap(~session)

pj_num <- .35
pj <- position_jitter(width = pj_num, height = pj_num)
ggplot(dc, aes(stim, rating, group = ss)) + 
  geom_point(position = pj, size = .5) +
  geom_line(stat = "smooth", method = "lm", se = FALSE, color = "black", alpha = 1/2, linewidth = .5) +
  theme_bw() +
  facet_wrap(~session)
  

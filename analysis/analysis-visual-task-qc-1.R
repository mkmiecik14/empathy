# analysis-visual-task-qc-1.R
# Matt Kmiecik
# 30 December 2024

# Purpose: perform quality checks on the visual task

# libraries ----
library(tidyverse); library(RColorBrewer)

# data ----
dd <- readRDS(file = "output/vis-task-data.rds")

# remove after these data have been cleaned
dc <- dd %>% filter(session %in% c(10:12), rating %in% c(0:20)) 

# histogram of ratings by session
ggplot(dc, aes(rating)) + 
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 20, 5)) +
  labs(x = "Rating", y = "Frequency") +
  facet_wrap(~session) + 
  theme_bw()

# boxplot by session
pj <- position_jitter(height = .5, width = .1)
s_cols <- brewer.pal(9, name = "PuBu")[c(6:8)]
names(s_cols) <- c(10:12)
ggplot(
  dc %>% mutate(session = factor(session)), 
  aes(session, rating, color = session, fill = session)
  ) + 
  geom_point(position = pj, alpha = 1/3, shape = 16) +
  geom_boxplot(width = .2, position = position_nudge(x = .3), color = "black") +
  labs(x = "Session", y = "Rating") +
  scale_color_manual(values = s_cols) +
  scale_fill_manual(values = s_cols) +
  theme_bw() +
  theme(legend.position = "none")

# boxplot by stimulus intensity
pj <- position_jitter(height = .5, width = .1)
stim_cols <- brewer.pal(9, name = "BuPu")[c(5:9)]
names(stim_cols) <- c(1:5)
ggplot(
  dc %>% mutate(stim = factor(stim)), 
  aes(stim, rating, group = stim, color = stim, fill = stim)
) + 
  geom_point(position = pj, alpha = 1/3, shape = 16) +
  geom_boxplot(width = .2, position = position_nudge(x = .3), color = "black") +
  labs(x = "Stimulus Intensity", y = "Rating") +
  scale_color_manual(values = stim_cols) +
  scale_fill_manual(values = stim_cols) +
  theme_bw() +
  facet_wrap(~session, ncol = 1) +
  theme(legend.position = "none")

# subject-wise slopes
pj_num <- .35
pj <- position_jitter(width = pj_num, height = pj_num)
ggplot(
  dc %>% mutate(session = factor(session)), 
  aes(stim, rating, group = ss, color = session)
  ) + 
  geom_point(position = pj, size = .5) +
  geom_line(
    stat = "smooth", method = "lm", se = FALSE, alpha = 1/2, 
    linewidth = .5
    ) +
  theme_bw() +
  labs(x = "Stimulus Intensity", y = " Rating") +
  scale_color_manual(values = s_cols) +
  facet_wrap(~session) +
  theme(legend.position = "none")

################
#              #
# SUMMARY DATA #
#              #
################

# summarises trial-wise
dc_trial_sum <- 
  dc %>% 
  summarise(
    m = mean(rating), sd = sd(rating), n = n(), sem = sd/sqrt(n), 
    .by = c(session, stim)
    )

# plot
ggplot(
  dc_trial_sum %>% mutate(session = factor(session)), 
  aes(stim, m, group = session, color = session)
  ) +
  geom_point(
    data = dc %>% mutate(session = factor(session)), 
    aes(y = rating), alpha = 1/3, shape = 16, position = pj, size = .75
    ) +
  geom_point() +
  geom_errorbar(aes(ymin = m-sem, ymax = m+sem), width = .1) +
  geom_line() +
  scale_color_manual(values = s_cols) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(
    x = "Stimulus Intensity", y = "Mean Rating", caption = "SEM error bars."
    ) +
  theme_bw() +
  facet_wrap(~session) +
  theme(legend.position = "none")
  

# analysis-long-pain.R
# Matt Kmiecik
# Started 2025-06-08
# Current version: v1
# Purpose: explore the longitudinal pain data

# libraries ----
library(tidyverse); library(glue); library(here)
library(lme4); library(lmerTest)
library(mgcv)

# custom functions ----
source("src/fns/broom_gam.R") # functions for extracting GAMs
source("src/fns/predict_link.R") # function for predicting on link function

# grabs versioning info ----
source("src/fns/versioning_proc.R")
vinfo <- versioning_proc(testing = FALSE, this_script = "analysis-long-pain")

# data ----
f <- file.path("output", "prepro-long-pain-v1.rds")
d <- read_rds(f)

# plotting raw data ----
d_long <- d %>% pivot_longer(cols = -c(subject_id:days_since_baseline))

# histogram
p1 <- 
  ggplot(d_long, aes(value)) + 
  geom_histogram(binwidth = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  labs(x = "Pain Rating", y = "Frequency") +
  facet_grid(name~redcap_event_name)

# line plot
p2 <- 
  ggplot(d_long, aes(days_since_baseline/365, value, group = subject_id)) +
  geom_point(alpha = .5) +
  geom_line(alpha = .5) +
  labs(x = "Years since PV1", y = "Pain Rating") +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  facet_wrap(~name) +
  theme_bw()

# gam ----

# prepares data for modeling
mod_data <- 
  d_long %>% 
  mutate(
    yrs_since_baseline = time_length(
      interval(baselinedate, todaysdate), unit = "years"
    )
  ) %>%
  mutate(subject_id = factor(subject_id))

# to run both ML for comparison and REML for inference
types <- c("ML", "REML") 
mod_res <- vector("list", length(types))
names(mod_res) <- types

for (i in 1:length(mod_res)) {
  ttype <- names(mod_res)[i]
  mod_res[[i]] <-
    mod_data %>% 
    nest_by(name) %>% 
    mutate(
      gam_gaus = list(
        gam(
          value ~ 1 + yrs_since_baseline + s(subject_id, bs = "re"), 
          data = data,
          family = gaussian(),
          method = ttype # ML or REML
        )
      ),
      gam_tw = list(
        gam(
          value ~ 1 + yrs_since_baseline + s(subject_id, bs = "re"), 
          data = data,
          family = Tweedie(p = 1.1),
          method = ttype # ML or REML
        )
      )
    )
}

# prediction ----

# creates new data
newdata <- 
  tibble(
    yrs_since_baseline = seq(0, 2, length.out = 200), 
    subject_id = "theoretical_obs"
  )

# preallocates
tw_preds <- vector("list", length(mod_res$REML$name))
names(tw_preds) <- mod_res$REML$name
gaus_preds <- tw_preds # also preallocates for gaussian

# loops and generates predictions
for (i in 1:length(tw_preds)) {
  tw_preds[[i]] <- 
    predict_link(mod = mod_res$REML$gam_tw[[i]], newdat = newdata)
  gaus_preds[[i]] <- 
    predict_link(mod = mod_res$REML$gam_gaus[[i]], newdat = newdata)
}

# converts to long format
tw_preds_long <- 
  tw_preds %>% 
  list_rbind(names_to = "name") %>% 
  as_tibble() 

gaus_preds_long <- 
  gaus_preds %>% 
  list_rbind(names_to = "name") %>% 
  as_tibble() 

# plots predictions ----

# Tweedie
p3 <- 
  ggplot(tw_preds_long, aes(yrs_since_baseline, pred_resp)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr_resp, ymax = upr_resp), alpha = 1/3) +
    coord_cartesian(ylim = c(0, 10)) +
    scale_y_continuous(breaks = seq(0, 10, 2)) +
    labs(
      x = "Time Since PV1 (years)", 
      y = "Fitted Pain Response", 
      caption = "95% CI shading.",
      title = "Tweedie Predictions"
    ) +
    theme_bw() +
    facet_wrap(~name)

# Gaussian
p4 <- 
  ggplot(gaus_preds_long, aes(yrs_since_baseline, pred_resp)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr_resp, ymax = upr_resp), alpha = 1/3) +
    coord_cartesian(ylim = c(0, 10)) +
    scale_y_continuous(breaks = seq(0, 10, 2)) +
    labs(
      x = "Time Since PV1 (years)", 
      y = "Fitted Pain Response", 
      caption = "95% CI shading.",
      title = "Gaussian Predictions"
    ) +
    theme_bw() +
    facet_wrap(~name)

# extracting estimates
param_gaus <- 
  mod_res %>% 
  map(~reframe(.x, tidy_gam(gam_gaus, type = "parametric"))) %>%
  list_rbind(names_to = "fit")

param_tw <- 
  mod_res %>% 
  map(~reframe(.x, tidy_gam(gam_tw, type = "parametric"))) %>%
  list_rbind(names_to = "fit")

# extracting omnibus estimates
glance_gaus <- 
  mod_res %>%
  map(~reframe(.x, glance_gam(gam_gaus))) %>%
  list_rbind(names_to = "fit")

glance_tw <- 
  mod_res %>%
  map(~reframe(.x, glance_gam(gam_tw))) %>%
  list_rbind(names_to = "fit")

# plots of AIC
pdata <- 
  bind_rows(
    glance_gaus %>% mutate(family = "Gaussian"), 
    glance_tw %>% mutate(family = "Tweedie")
  ) %>%
  filter(fit == "ML")
p5 <- 
  ggplot(pdata, aes(family, AIC)) +
    geom_bar(stat = "identity", color = "black", fill = "grey", width = .5) +
    labs(x = "Distribution/Family") +
    theme_bw() +
    facet_wrap(~name)

# writing out ----

# grabbing variables to save out
v <- 
  c(
    paste0("p", 1:5), "mod_res", "tw_preds_long", "gaus_preds_long", 
    ls(pattern = "^param"), "glance_gaus", "glance_tw"
    )
res <- mget(v) # creates list
versioned_write_rds(data = res, vi = vinfo) # writes out



# analysis-bladder-task-gam-1.R
# Matt Kmiecik
# Started 2025-05-15

# Purpose: using GAMMs to analyze the bladder task

# libraries ----
library(tidyverse); library(mgcv); library(itsadug)

# data ----
f <- file.path("output", "bladder-task-long-data.rds")
d <- read_rds(file = f)

# proc ----

# gathers ids for sampling
ids <- unique(d$subject_id)

## plotting participant performance
# plot data
pdata <- 
  d %>% 
  filter(
    redcap_event_name == "baseline_visit_chi_arm_1", 
    subject_id %in% ids[1:30],
    !is.na(diff_time), !is.na(pain))
# plot
ggplot(
  pdata, 
  aes(diff_time, pain, group = subject_id)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_wrap(~subject_id)

# histogram of pain responses
ggplot(pdata, aes(pain)) + geom_histogram(binwidth = 2)
ggplot(pdata, aes(urgency)) + geom_histogram(binwidth = 2)

# Generalized additive mixed models ----

# modeling data (looking at baseline)
mod_data <- 
  d %>% 
  filter(redcap_event_name == "baseline_visit_chi_arm_1") %>%
  mutate(subject_id = as.factor(subject_id))

# random intercept model with linear effect of time
mod1 <- gam(pain ~ 1 + diff_time + s(subject_id, bs = "re"), data = mod_data)
summary(mod1)
plot(mod1)
qq.gam(mod1)

# random intercepts and slopes, with linear effect of time
mod2 <- 
  gam(
    pain ~ 1 + diff_time + s(subject_id, bs = "re") + 
      s(subject_id, diff_time, bs = "re"), 
    data = mod_data
  )
summary(mod2)
plot(mod2, pages = 1)
qq.gam(mod2)
anova(mod1, mod2) # model comparison


# random slopes, with non-linear effect of time
mod3 <- bam(pain ~ 1 + s(diff_time) + s(subject_id, bs = "re"), data = mod_data)
summary(mod3)
plot(mod3, pages = 1)
qq.gam(mod3)
anova(mod1, mod2, mod3) # model comparison

# model 4 seems nice! I think this one is the winner
mod4_gaus <- 
  bam(
    pain ~ 1 + s(diff_time) + s(subject_id, bs = "re") + 
      s(subject_id, diff_time, bs = "re"), 
    data = mod_data, # %>% filter(subject_id %in% ids[1:50])
    #family = tw(link = "log"), # was initially run without this
    method = "fREML",
    control = gam.control(trace = TRUE)
  )
summary(mod4_gaus)
plot(mod4_gaus, pages = 1)
qq.gam(mod4_gaus)
anova(mod1, mod2, mod3, mod4_gaus)
AIC(mod1, mod2, mod3, mod4_gaus)

# running model 4 again but with Tweedie distribution
mod4_tw <- 
  bam(
    pain ~ 1 + s(diff_time) + s(subject_id, bs = "re") + 
      s(subject_id, diff_time, bs = "re"), 
    data = mod_data, # %>% filter(subject_id %in% ids[1:50])
    family = tw(link = "log"), # was initially run without this
    method = "fREML",
    control = gam.control(trace = TRUE)
  )
summary(mod4_tw)
plot(mod4_tw, pages = 1)
qq.gam(mod4_tw)
#anova(mod1, mod2, mod3, mod4_gaus, mod4_tw)
AIC(mod1, mod2, mod3, mod4_gaus, mod4_tw)

# also looking at urgency
mod4_tw_x <- 
  bam(
    pain ~ 1 + te(diff_time, urgency) + s(subject_id, bs = "re") + 
      s(subject_id, diff_time, bs = "re"), 
    data = mod_data, # %>% filter(subject_id %in% ids[1:50])
    family = tw(link = "log"), # was initially run without this
    method = "fREML",
    control = gam.control(trace = TRUE),
    select = TRUE
  )
summary(mod4_tw_x)
plot(mod4_tw_x, pages = 1)
vis.gam(
  mod4_tw_x, 
  view = c("diff_time", "urgency"), 
  plot.type = "persp", 
  se = 0, 
  type = "response", 
  theta=30, phi=20
)
gratia::draw(mod4_tw_x)
qq.gam(mod4_tw_x)
anova(mod4_tw, mod4_tw_x)

# modeling main effects and smooth interaction
k <-7
mod4_tw_x2 <- 
  bam(
    pain ~ 1 + s(diff_time, k = k) + s(urgency, k = k) + ti(diff_time, urgency, k = k) + 
      s(subject_id, bs = "re") + 
      s(subject_id, diff_time, bs = "re"), 
    data = mod_data, # %>% filter(subject_id %in% ids[1:50])
    family = tw(link = "log"), # was initially run without this
    method = "fREML",
    control = gam.control(trace = TRUE),
    select = TRUE
  )
summary(mod4_tw_x2)
plot(mod4_tw_x2, pages = 1, scale = 0, scheme = 1)
vis.gam(
  mod4_tw_x2, 
  view = c("diff_time", "urgency"), 
  plot.type = "persp", 
  se = 0, 
  type = "response", 
  theta=30, phi=20
  )
vis.gam(
  mod4_tw_x2, 
  view = c("diff_time", "urgency"), 
  plot.type = "contour",
  type = "response"
)
anova(mod4_tw_x, mod4_tw_x2)
AIC(mod4_tw_x, mod4_tw_x2)
compareML(mod4_tw_x, mod4_tw_x2)
concurvity(mod4_tw_x2)

# prediction ----
new_data <- 
  with(
    mod_data, 
    expand.grid(
      subject_id = "theoretical",
      diff_time = seq(0, max(diff_time, na.rm = TRUE), length.out = 200),
      urgency = c(mean(urgency, na.rm = TRUE), 50)
    )
  )
preds <- predict(mod4_tw_x2, newdata = new_data, type = "link", se.fit=TRUE)
new_data_2 <- 
  cbind(new_data, preds) %>%
  as_tibble() %>%
  mutate(upr = fit + (2 * se.fit), lwr = fit - (2 * se.fit)) %>%
  mutate(
    across(
      .cols = c(fit, se.fit, upr, lwr), .fns = ~mod4_tw_x2$family$linkinv(.x), 
      .names = "resp_{.col}"
      )
    )

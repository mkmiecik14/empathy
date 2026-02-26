# analysis-long-supp-proj-simplified.R
# Matt Kmiecik
# Current version: v1
# Purpose: compute MMH longitudinally via supplementary projections and examine
# change over time; script is simplified to focus on linear mixed model w/o
# smooth terms

# grabs versioning info ----
source("src/fns/versioning_proc.R")
vinfo <- 
  versioning_proc(
    testing = FALSE, 
    this_script = "analysis-long-supp-proj-simplified"
    )

# libraries ----
library(tidyverse); library(patchwork); library(mgcv)
RDGY <- RColorBrewer::brewer.pal(11, "RdGy")

# custom functions ----
source("src/fns/broom_gam.R")
source("src/fns/predict_link.R")

# data ----
f <- file.path("output", "pca-3-fi-supp-proj-mod-data.rds")
d <- read_rds(f)

# proc ----

VISIT <- 1 # visit to pull the menstrual and pelvic pain from
if (!VISIT %in% 1:2) {
  stop("Visit for menstrual and pelvic pain must be either 1 or 2...\n")
} else{
  cat(sprintf("Using menstrual and pelvic pain from visit %s.\n", VISIT))
}

# creating modeling data frame
mod_data <- 
  d$exp_pca %>% 
  left_join(., d$tanner, by = c("subject_id", "visit")) %>%
  left_join(., d$pain, by = c("subject_id", "visit")) %>% # will fail modeling
  mutate(subject_id = factor(subject_id)) %>%
  # simplifying data
  select(
    subject_id, visit, yrs_since_baseline, starts_with("tanner"), menst_pain, 
    pelvic_pain, all_of(paste0("PC", 1:3))
    ) %>%
  # given that menstrual is not possible to collect at baseline (pre-menarchal visit)
  # menstrual pain and pelvic pain will be the second visit
  mutate(
    across(
      .cols = c(menst_pain, pelvic_pain),
      .fns = ~if_else(any(visit == VISIT), .x[visit == VISIT][1], NA),
      .names = "{.col}_pv"
    ),
    .by = subject_id
    )

# random intercepts
mod <- 
  gam(
    PC1 ~ 1 + 
      tanner_breast + tanner_hair + # adjust for development
      yrs_since_baseline*menst_pain_pv*pelvic_pain_pv + 
      s(subject_id, bs = "re"), 
    data = mod_data, 
    method = "REML"
    )
summary(mod)
plot(mod, pages = 1)
gam.check(mod)

# helper function to organize results
org_mod_res <- function(model){
  res <- list(
    mod = model,
    sum = summary(model),
    omni = glance_gam(model), 
    est_p = tidy_gam(model, type = "parametric"),
    est_s = tidy_gam(model, type = "smooth")
  )
  return(res)
}
mod1_res <- org_mod_res(mod)


# random slopes
mod2 <- 
  gam(
    PC1 ~ 1 + 
      tanner_breast + tanner_hair + # adjust for development
      yrs_since_baseline*menst_pain_pv*pelvic_pain_pv + 
      s(subject_id, bs = "re") +
      s(subject_id, yrs_since_baseline, bs = "re"), 
    data = mod_data, 
    method = "REML"
  )
summary(mod2)
plot(mod2, pages = 1)
gam.check(mod2)
mod2_res <- org_mod_res(mod2)


AIC(mod, mod2)
itsadug::compareML(mod, mod2)

# generating predictions

# effect of yrs_since_baseline
newdata <- 
  with(
  mod$model, 
  expand.grid(
    subject_id = "theoretical_subject",
    tanner_breast = mean(tanner_breast),
    tanner_hair = mean(tanner_hair),
    yrs_since_baseline = seq(
      min(yrs_since_baseline), max(yrs_since_baseline), length.out = 200
      ),
    menst_pain_pv = mean(menst_pain_pv),
    pelvic_pain_pv = mean(pelvic_pain_pv)
  )
  )

# effect of pelvic pain
newdata2 <- 
  with(
    mod$model, 
    expand.grid(
      subject_id = "theoretical_subject",
      tanner_breast = mean(tanner_breast),
      tanner_hair = mean(tanner_hair),
      yrs_since_baseline = mean(yrs_since_baseline),
      menst_pain_pv = mean(menst_pain_pv),
      pelvic_pain_pv = seq(
        min(pelvic_pain_pv), max(pelvic_pain_pv), length.out = 200
      )
    )
  )


# predictions for effect of yrs_since_baseline
pred1 <- predict_link(mod = mod, newdat = newdata)
p1 <- 
  ggplot(pred1, aes(yrs_since_baseline, pred_resp)) +
  geom_ribbon(aes(ymin = lwr_resp, ymax = upr_resp), alpha = 1/3) +
  geom_line() +
  labs(x = "Years Since Baseline Visit", y = "MMH (PC1)") +
  theme_bw()
p1

# predictions for the effect of pelvic_pain_pv1
pred2 <- predict_link(mod = mod, newdat = newdata2)
p2 <- 
  ggplot(pred2, aes(pelvic_pain_pv, pred_resp)) +
  geom_ribbon(aes(ymin = lwr_resp, ymax = upr_resp), alpha = 1/3) +
  geom_line() +
  labs(x = "Pelvic Pain", y = "MMH (PC1)") +
  theme_bw()
p2

# examining raw change in PC1 over time
mod1_data <- as_tibble(mod$model, rownames = "r")
mod1_data$visit <- mod_data[mod1_data$r,]$visit
p3 <- 
  ggplot(mod1_data, aes(yrs_since_baseline, PC1, group = subject_id)) +
  geom_line(color = RDGY[8]) +
  geom_smooth(method = "lm", aes(group = 1), color = RDGY[3], se = TRUE, fill = RDGY[10]) +
  labs(x = "Years Since Baseline", y = "Multimodal Hypersensitivity (PC1)") +
  theme_bw()
p3

# boxplot
p4 <- 
  ggplot(mod1_data, aes(factor(visit), PC1, fill = factor(visit))) +
  geom_boxplot(width = .5) +
  labs(x = "Visit", y = "Multimodal Hypersensitivity (PC1)") +
  scale_fill_brewer(palette = "Oranges") +
  theme_bw() +
  theme(legend.position = "none")
p4

# Observed and predicted quartiles for pelvic pain (and menstrual pain) ----
pain_quartiles <- 
  mod1_data %>% 
  select(subject_id, menst_pain_pv, pelvic_pain_pv) %>% 
  distinct() %>%
  mutate(
    across(
      .cols = contains("pain"), 
      .fns = ~ntile(.x, 4), 
      .names = "{.col}_q")
    )

# quartile boundaries 
quartile_boundaries <- 
  list(
    menst_pain = quantile(pain_quartiles$menst_pain_pv, probs = c(0, 0.25, 0.5, 0.75, 1)),
    pelvic_pain = quantile(pain_quartiles$pelvic_pain_pv, probs = c(0, 0.25, 0.5, 0.75, 1))
)

# long-format subject-wise pain quartiles for observed plot
pain_quartiles_long <- 
  mod1_data %>% 
  left_join(
    ., 
    pain_quartiles, 
    join_by(subject_id, menst_pain_pv, pelvic_pain_pv)
    ) %>%
  select(subject_id, visit, ends_with("q"), PC1) %>%
  pivot_longer(cols = -c(subject_id, visit, PC1)) %>%
  mutate(value = case_when(
    value == 1 ~ "low",
    value %in% c(2:3) ~ "middle",
    value == 4 ~ "high",
    .default = NA
  )
  )

# long-format summary stats pain quartiles for observed plot
pain_quartiles_sum <- 
  pain_quartiles_long %>% 
  summarise(
    m = mean(PC1), sd = sd(PC1), n = n(), sem = sd/sqrt(n), 
    ll = m - qt(0.975, n-1)*sem, ul = m + qt(0.975, n-1)*sem,
    .by = c(visit, name, value))

# PREDICTING QUARTILE BOUNDARIES 

# list to store probabilities
# note: these are different between menstrual and pelvic pain as the 0% and 25%
# boundaries are both zero for pelvic pain
this_probs <- list(
  pelvic_pain = c(.25, .5, .75, 1),
  menst_pain = seq(0, 1, .25)
)

# quartile boundaries of pelvic pain (keeping constant everything else)
newdata_pelvic_q <- 
  with(
    mod$model, 
    expand.grid(
      subject_id = "theoretical_subject",
      tanner_breast = mean(tanner_breast),
      tanner_hair = mean(tanner_hair),
      yrs_since_baseline = seq(
        min(yrs_since_baseline), max(yrs_since_baseline), length.out = 200
      ),
      menst_pain_pv = mean(menst_pain_pv),
      # upper limits of quartiles
      pelvic_pain_pv = quantile(pelvic_pain_pv, probs = this_probs$pelvic_pain)
      )
    )
# quartile boundaries of menstrual pain (keeping constant everything else)
newdata_menst_q <- 
  with(
    mod$model, 
    expand.grid(
      subject_id = "theoretical_subject",
      tanner_breast = mean(tanner_breast),
      tanner_hair = mean(tanner_hair),
      yrs_since_baseline = seq(
        min(yrs_since_baseline), max(yrs_since_baseline), length.out = 200
      ),
      # upper limits of quartiles (boundaries)
      menst_pain_pv = quantile(menst_pain_pv, probs = this_probs$menst_pain),
      pelvic_pain_pv = mean(pelvic_pain_pv)
    )
  )

# predictions:
quartile_preds <- 
  list(pelvic_pain = newdata_pelvic_q, menst_pain = newdata_menst_q) %>%
  map(~predict_link(mod = mod, newdat = .x)) %>%
  map(~as_tibble(.x))

# saving out results ----
res <- list(
  visit = VISIT,
  raw_data = mod_data,
  model_res = mget(paste0("mod", 1:2, "_res")),
  plots = mget(paste0("p", 1:4)),
  preds = list(time = pred1, pelvic_pain = pred2),
  quartile_boundaries = quartile_boundaries,
  obs_quartiles_ss = pain_quartiles_long, # observed quartiles subject-wise
  obs_quartiles_sum = pain_quartiles_sum, # observed quartiles summary stats
  prediction_probs = this_probs, # probabilities used for quartile prediction
  quartile_preds = quartile_preds # generated predictions from quartile boundaries
)

# saving ----
versioned_write_rds(data = res, vi = vinfo)

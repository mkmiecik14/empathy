# analysis-mixed-models.R
# Matt Kmiecik
# Purpose: examine change in MMH over time using linear mixed models (lme4);
# random slopes with correlated intercepts preferred over mgcv due to strong
# intercept-slope correlation

# LIBRARIES ====================================================================
library(tidyverse); library(patchwork); library(glue)
library(lme4); library(lmerTest)
RDGY <- RColorBrewer::brewer.pal(11, "RdGy")

# CONFIG =======================================================================
CFG <- list(script = "analysis-mixed-models")

# DATA =========================================================================
f <- file.path("output", "prepro", "pca-3-fi-supp-proj-mod-data.rds")
d <- read_rds(f)

# PROCEDURE ====================================================================
VISIT <- 1
if (!VISIT %in% 1:2) {
  stop("Visit for menstrual and pelvic pain must be either 1 or 2...\n")
} else {
  cat(sprintf("Using menstrual and pelvic pain from visit %s.\n", VISIT))
}

# creating modeling data frame
mod_data <-
  d$exp_pca %>%
  left_join(., d$tanner, by = c("subject_id", "visit")) %>%
  left_join(., d$pain, by = c("subject_id", "visit")) %>%
  mutate(subject_id = factor(subject_id)) %>%
  select(
    subject_id, visit, yrs_since_baseline, starts_with("tanner"), menst_pain,
    pelvic_pain, all_of(paste0("PC", 1:3))
  ) %>%
  mutate(
    across(
      .cols = c(menst_pain, pelvic_pain),
      .fns = ~if_else(any(visit == VISIT), .x[visit == VISIT][1], NA),
      .names = "{.col}_pv"
    ),
    .by = subject_id
  ) %>%
  # between subject effect of pubertal development
  mutate(
    tanner_breast_pm = mean(tanner_breast, na.rm = TRUE),
    tanner_hair_pm   = mean(tanner_hair,   na.rm = TRUE),
    .by = subject_id
  ) %>%
  # within subject change in pubertal development
  mutate(
    tanner_breast_wc = tanner_breast - tanner_breast_pm,
    tanner_hair_wc   = tanner_hair   - tanner_hair_pm
  )

# data to compute grandmean
gm_data <- mod_data %>% distinct(subject_id, tanner_breast_pm, tanner_hair_pm)
tanner_breast_gm <- mean(gm_data$tanner_breast_pm)
tanner_hair_gm <- mean(gm_data$tanner_hair_pm)

# grandmean centered data
mod_data$tanner_hair_gmc <- mod_data$tanner_hair_pm - tanner_hair_gm
mod_data$tanner_breast_gmc <- mod_data$tanner_breast_pm - tanner_breast_gm

# mod_data %>% select(subject_id, contains("tanner")) %>% filter(subject_id == unique(mod_data$subject_id)[1])

# RANDOM INTERCEPT MODEL =======================================================
fit1 <- lmer(
  PC1 ~ 1 +
    tanner_breast_gmc + tanner_breast_wc + tanner_hair_gmc + tanner_hair_wc +
    yrs_since_baseline * menst_pain_pv * pelvic_pain_pv +
    (1 | subject_id),
  data = mod_data,
  REML = TRUE
)
summary(fit1)
plot(fit1)

# RANDOM INTERCEPTS + RANDOM SLOPES MODEL ======================================
fit2 <- lmer(
  PC1 ~ 1 +
    tanner_breast_gmc + tanner_breast_wc + tanner_hair_gmc + tanner_hair_wc +
    yrs_since_baseline * menst_pain_pv * pelvic_pain_pv +
    (1 + yrs_since_baseline | subject_id),
  data = mod_data,
  REML = TRUE
)
summary(fit2)
plot(fit2)

# MODEL COMPARISON =============================================================
# model comparison (REML valid: same fixed effects, different random structure)
anova(fit1, fit2, refit = FALSE)

# PREDICTION ===================================================================

# helper: population-level predictions with fixed-effects 95% CIs ----
predict_lmer <- function(model, newdat) {
  fe_formula <- formula(model, fixed.only = TRUE)
  mm <- model.matrix(fe_formula[-2], data = newdat)
  pred <- as.vector(mm %*% fixef(model))
  se <- sqrt(diag(mm %*% tcrossprod(vcov(model), mm)))
  crit_val <- qt(0.975, df = df.residual(model))
  as_tibble(newdat) %>%
    mutate(pred = pred, se = se, lwr = pred - crit_val * se, upr = pred + crit_val * se)
}

# prediction grids (using fitted data to respect observed ranges) ----
fit2_data <- model.frame(fit2)

# reference values: one row per subject to avoid visit-weighting; 0 for _wc
tanner_ref <- 
  fit2_data %>%
  distinct(
    subject_id, tanner_breast_gmc, tanner_hair_gmc, menst_pain_pv, pelvic_pain_pv
  ) %>%
  summarise(
    menst_pain_pv     = mean(menst_pain_pv,     na.rm = TRUE),
    pelvic_pain_pv    = mean(pelvic_pain_pv,    na.rm = TRUE)
  ) %>%
  as.list() %>%
  c(
    tanner_breast_wc = 0, 
    tanner_hair_wc = 0, 
    tanner_breast_gmc = 0, 
    tanner_hair_gmc = 0  
  )

# effect of yrs_since_baseline
newdata <-
  with(
    fit2_data,
    expand.grid(
      tanner_breast_gmc = tanner_ref$tanner_breast_gmc,
      tanner_breast_wc  = tanner_ref$tanner_breast_wc,
      tanner_hair_gmc   = tanner_ref$tanner_hair_gmc,
      tanner_hair_wc    = tanner_ref$tanner_hair_wc,
      yrs_since_baseline = seq(
        min(yrs_since_baseline), max(yrs_since_baseline), length.out = 200
      ),
      menst_pain_pv  = tanner_ref$menst_pain_pv,
      pelvic_pain_pv = tanner_ref$pelvic_pain_pv
    )
  )

# effect of pelvic pain
newdata2 <-
  with(
    fit2_data,
    expand.grid(
      tanner_breast_gmc = tanner_ref$tanner_breast_gmc,
      tanner_breast_wc  = tanner_ref$tanner_breast_wc,
      tanner_hair_gmc   = tanner_ref$tanner_hair_gmc,
      tanner_hair_wc    = tanner_ref$tanner_hair_wc,
      yrs_since_baseline = mean(yrs_since_baseline),
      menst_pain_pv  = tanner_ref$menst_pain_pv,
      pelvic_pain_pv = seq(min(pelvic_pain_pv), max(pelvic_pain_pv), length.out = 200)
    )
  )

# effect of menstrual pain
newdata3 <-
  with(
    fit2_data,
    expand.grid(
      tanner_breast_gmc = tanner_ref$tanner_breast_gmc,
      tanner_breast_wc  = tanner_ref$tanner_breast_wc,
      tanner_hair_gmc   = tanner_ref$tanner_hair_gmc,
      tanner_hair_wc    = tanner_ref$tanner_hair_wc,
      yrs_since_baseline = mean(yrs_since_baseline),
      menst_pain_pv  = seq(min(menst_pain_pv), max(menst_pain_pv), length.out = 200),
      pelvic_pain_pv = tanner_ref$pelvic_pain_pv
    )
  )

# generate predictions
pred1 <- predict_lmer(fit2, newdata) # time
pred2 <- predict_lmer(fit2, newdata2) # pelvic pain
pred3 <- predict_lmer(fit2, newdata3) # menstrual pain


# plots ----
p1 <-
  ggplot(pred1, aes(yrs_since_baseline, pred)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 1/3) +
  geom_line() +
  labs(x = "Years Since Baseline Visit", y = "MMH (PC1)") +
  theme_bw()
p1

p2 <-
  ggplot(pred2, aes(pelvic_pain_pv, pred)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 1/3) +
  geom_line() +
  labs(x = "Pelvic Pain", y = "MMH (PC1)") +
  theme_bw()
p2

p2_m <- 
  ggplot(pred3, aes(menst_pain_pv, pred)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 1/3) +
  geom_line() +
  labs(x = "Menstrual Pain", y = "MMH (PC1)") +
  theme_bw()
p2_m

# raw subject-level trajectories
p3 <-
  ggplot(mod_data, aes(yrs_since_baseline, PC1, group = subject_id)) +
  geom_line(color = RDGY[8]) +
  geom_smooth(method = "lm", aes(group = 1), color = RDGY[3], se = TRUE, fill = RDGY[10]) +
  labs(x = "Years Since Baseline", y = "Multimodal Hypersensitivity (PC1)") +
  theme_bw()
p3

# boxplot by visit
p4 <-
  ggplot(mod_data, aes(factor(visit), PC1, fill = factor(visit))) +
  geom_boxplot(width = .5) +
  labs(x = "Visit", y = "Multimodal Hypersensitivity (PC1)") +
  scale_fill_brewer(palette = "Oranges") +
  theme_bw() +
  theme(legend.position = "none")
p4

# observed quartiles for pelvic and menstrual pain ----
pain_quartiles <-
  mod_data %>%
  select(subject_id, menst_pain_pv, pelvic_pain_pv) %>%
  distinct() %>%
  mutate(
    across(
      .cols = contains("pain"),
      .fns = ~ntile(.x, 4),
      .names = "{.col}_q"
    )
  )

quartile_boundaries <- list(
  menst_pain = quantile(
    pain_quartiles$menst_pain_pv, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE
  ),
  pelvic_pain = quantile(
    pain_quartiles$pelvic_pain_pv, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE
  )
)

pain_quartiles_long <-
  mod_data %>%
  left_join(pain_quartiles, join_by(subject_id, menst_pain_pv, pelvic_pain_pv)) %>%
  select(subject_id, visit, ends_with("q"), PC1) %>%
  pivot_longer(cols = -c(subject_id, visit, PC1)) %>%
  mutate(value = case_when(
    value == 1 ~ "low",
    value %in% c(2:3) ~ "middle",
    value == 4 ~ "high",
    .default = NA
  ))

pain_quartiles_sum <-
  pain_quartiles_long %>%
  summarise(
    m = mean(PC1), sd = sd(PC1), n = n(), sem = sd / sqrt(n),
    ll = m - qt(0.975, n - 1) * sem, ul = m + qt(0.975, n - 1) * sem,
    .by = c(visit, name, value)
  )

# predictions at quartile boundaries ----
this_probs <- list(
  pelvic_pain = c(.25, .5, .75, 1),
  menst_pain = seq(0, 1, .25)
)

newdata_pelvic_q <-
  with(
    fit2_data,
    expand.grid(
      tanner_breast_gmc = tanner_ref$tanner_breast_gmc,
      tanner_breast_wc  = tanner_ref$tanner_breast_wc,
      tanner_hair_gmc   = tanner_ref$tanner_hair_gmc,
      tanner_hair_wc    = tanner_ref$tanner_hair_wc,
      yrs_since_baseline = seq(min(yrs_since_baseline), max(yrs_since_baseline), length.out = 200),
      menst_pain_pv  = tanner_ref$menst_pain_pv,
      pelvic_pain_pv = quantile(pelvic_pain_pv, probs = this_probs$pelvic_pain)
    )
  )

newdata_menst_q <-
  with(
    fit2_data,
    expand.grid(
      tanner_breast_gmc = tanner_ref$tanner_breast_gmc,
      tanner_breast_wc  = tanner_ref$tanner_breast_wc,
      tanner_hair_gmc   = tanner_ref$tanner_hair_gmc,
      tanner_hair_wc    = tanner_ref$tanner_hair_wc,
      yrs_since_baseline = seq(min(yrs_since_baseline), max(yrs_since_baseline), length.out = 200),
      menst_pain_pv  = quantile(menst_pain_pv, probs = this_probs$menst_pain),
      pelvic_pain_pv = tanner_ref$pelvic_pain_pv
    )
  )

quartile_preds <-
  list(pelvic_pain = newdata_pelvic_q, menst_pain = newdata_menst_q) %>%
  map(~predict_lmer(model = fit2, newdat = .x))

# saving results ----
res <- list(
  visit = VISIT,
  raw_data = mod_data,
  models = list(fit1 = fit1, fit2 = fit2),
  model_comparison = anova(fit1, fit2, refit = FALSE),
  plots = mget(c(paste0("p", 1:4), "p2_m")),
  preds = list(time = pred1, pelvic_pain = pred2, menst_pain = pred3),
  quartile_boundaries = quartile_boundaries,
  obs_quartiles_ss = pain_quartiles_long,
  obs_quartiles_sum = pain_quartiles_sum,
  prediction_probs = this_probs,
  quartile_preds = quartile_preds
)

write_rds(res, file.path("output", "analysis", glue("{CFG$script}.rds")))

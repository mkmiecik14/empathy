# analysis-long-mmh.R
# Matt Kmiecik
# Started 2025-06-15
# Current version: v1
# Purpose: combine longitudinal pain assessment with PCs

# Notes:
# Support for inverse hyperbolic sine transformation for postiively skewed data
# https://stats.stackexchange.com/questions/1444/how-should-i-transform-non-negative-data-including-zeros

# libraries ----
library(tidyverse)

# custom functions ----
source("src/fns/predict_link.R")
source("src/fns/broom_gam.R")

# grabs versioning info ----
source("src/fns/versioning_proc.R")
vinfo <- versioning_proc(testing = FALSE, this_script = "analysis-long-mmh")

# data ----

## longitudinal pain data
f <- file.path("output", "prepro-long-pain-v1.rds")
d <- read_rds(f)

## PCA results
f <- file.path("output", "analysis-pca-3-q-res.rds")
pca_res <- read_rds(f)
fi <- 
  pca_res$Fixed.Data$ExPosition.Data$fi %>% 
  as_tibble(rownames = "subject_id") %>%
  mutate(subject_id = as.double(subject_id))
colnames(fi) <- gsub("V", "PC", colnames(fi)) # changes to PC for clarity

## combines PCA and longitudinal pain data
comb_data <- full_join(d, fi, by = "subject_id")

#######################
#                     #
# painrating_child_q1 #
#                     #
#######################

# preps data
q1_data <- 
  comb_data %>% 
  select(
    subject_id, redcap_event_name, todaysdate, baselinedate, painrating_child_q1,
    starts_with("PC")
    ) %>%
  filter(!is.na(PC1), !is.na(painrating_child_q1)) %>%
  mutate(
    yrs_since_baseline = time_length(
      interval(baselinedate, todaysdate), unit = "years"
    ),
    .before = painrating_child_q1
  ) %>%
  mutate(subject_id = factor(subject_id))
q1_data %>% count(subject_id) %>% count(n) # n=107 that have 2 timepoints

# linear regression for PV1
mod_data <- q1_data %>% filter(grepl("visit_1", redcap_event_name))
lm_1 <- lm(painrating_child_q1 ~ 1 + PC1 + PC2 + PC3, data = mod_data)
summary(lm_1)

# linear mixed modeling 
library(mgcv)
mod1 <- 
  gam(
    painrating_child_q1 ~ 1 + yrs_since_baseline*(PC1 + PC2 + PC3) + 
      s(subject_id, bs = "re"),
    data = q1_data
    )
summary(mod1)
gam.check(mod1)
qq.gam(mod1)

# Base R QQ plot
resid_vals <- residuals(mod1) # extracts residuals
qqnorm(resid_vals)
qqline(resid_vals)

# prediction
mod1_data <- model.frame(mod1) # grabs data from model
new_data <- 
  with(
    mod1_data, 
    expand.grid(
      subject_id = "theoretical_obs",
      yrs_since_baseline = seq(0, max(yrs_since_baseline), length.out = 200),
      PC1 = c(mean(PC1), mean(PC1) - 2*sd(PC1), mean(PC1) + 2*sd(PC1)),
      PC2 = c(mean(PC2), mean(PC2) - 2*sd(PC2), mean(PC2) + 2*sd(PC2)),
      PC3 = c(mean(PC3), mean(PC3) - 2*sd(PC3), mean(PC3) + 2*sd(PC3))
      )
    )
pred <- predict_link(mod = mod1, newdat = new_data) # generates predictions

# plot data
pdata <- 
  pred %>% 
  mutate(
    PC1_SD = case_when(
      PC1 < unique(new_data$PC1)[1] ~ "+2SD",
      PC1 > unique(new_data$PC1)[1] ~ "-2SD",
      PC1 == unique(new_data$PC1)[1] ~ "mean",
      .default = NA
    )
    ) %>% 
  mutate(PC1_SD = factor(PC1_SD, levels = c("+2SD", "mean", "-2SD"))) %>%
  filter(PC2 %in% unique(pred$PC2)[1], PC3 %in% unique(pred$PC3)[1])

# plot
pcs <- palette.colors()
tcols <- c("+2SD" = pcs[2], "mean" = pcs[9], "-2SD" = pcs[3])
ggplot(
  pdata, 
  aes(
    yrs_since_baseline, pred_resp, group = PC1_SD, color = PC1_SD, fill = PC1_SD
    )
  ) +
  geom_ribbon(aes(ymin = lwr_resp, ymax = upr_resp), alpha = 1/2) +
  geom_line() +
  coord_cartesian(ylim = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  scale_color_manual(values = tcols) +
  scale_fill_manual(values = tcols) +
  labs(
    x = "Years Since PV1", 
    y = "Predicted Response", 
    title = "painrating_child_q1",
    caption = "95% CI error shading"
    ) +
  theme_bw()

#########################
#                       #
# pelvic pain composite #
#                       #
#########################

# preps data
c_data <- 
  comb_data %>% 
  select(
    subject_id, redcap_event_name, todaysdate, baselinedate, 
    matches("q2|q3|q4"), starts_with("PC")
  ) %>%
  mutate(
    yrs_since_baseline = time_length(
      interval(baselinedate, todaysdate), unit = "years"
    ),
    .after = baselinedate
  ) %>%
  mutate(subject_id = factor(subject_id)) %>%
  filter(complete.cases(.)) %>%
  rowwise() %>%
  mutate(pelvic_pain = mean(c_across(matches("q2|q3|q4")))) %>%
  ungroup() %>%
  # work to calculate composite using inverse hyperbolic sine transformation
  # using asinh()
  mutate(
    across(.cols = matches("q2|q3|q4"), .fns = ~asinh(.x), .names = "{.col}_trans") 
  ) %>%
  rowwise() %>%
  mutate(pelvic_pain_trans = mean(c_across(ends_with("trans"))))
c_data %>% count(subject_id) %>% count(n) # n=107 that have 2 timepoints

# plots of original and transformed variables
c_data_long <- 
  c_data %>% 
  select(-starts_with("PC")) %>% 
  pivot_longer(cols = -c(subject_id:yrs_since_baseline))

# histogram
pdata <- c_data_long %>% filter(!grepl("trans", name))
o_hist <-
  ggplot(pdata, aes(value)) +
    geom_histogram(binwidth = 1) +
    coord_cartesian(xlim = c(0, 10)) +
    scale_x_continuous(breaks = seq(0, 10, 1), minor_breaks = NULL) +
    facet_grid(name~redcap_event_name) +
    labs(x = "Pain Rating", y = "Frequency", title = "Original Values") +
    theme_bw()
o_hist

pdata <- c_data_long %>% filter(grepl("trans|pelvic_pain", name))
t_hist <- 
  ggplot(pdata, aes(value)) +
    geom_histogram(binwidth = 1) +
    coord_cartesian(xlim = c(0, 10)) +
    scale_x_continuous(breaks = seq(0, 10, 1), minor_breaks = NULL) +
    facet_grid(name~redcap_event_name) +
    labs(x = "Pain Rating", y = "Frequency", title = "Transformed") +
    theme_bw()
t_hist

# linear modeling
lm_mods <- 
  c_data %>% 
  nest_by(redcap_event_name) %>% 
  mutate(
    mod_o = list(lm(pelvic_pain ~ 1 + PC1 + PC2 + PC3, data = data)),
    mod_t = list(lm(pelvic_pain_trans ~ 1 + PC1 + PC2 + PC3, data = data))
    )
lm_mods %>% reframe(broom::tidy(mod_o, conf.int = TRUE))

# linear mixed modeling

# function for modeling
model_pelvic_gam <- 
  function(resp = "pelvic_pain", fam = gaussian(), data = NULL, ...){
    fixed <- "yrs_since_baseline*(PC1 + PC2 + PC3)"
    smooths <- "s(subject_id, bs = 're')"
    rhs <- c(fixed, smooths)
    formula_string <- paste(resp, "~", paste(rhs, collapse = " + "))
    gam_formula <- as.formula(formula_string)
    mod <- gam(gam_formula, data = data, family = fam, ...)
    return(mod)
}

# preallocations
meas <- c("pelvic_pain", "pelvic_pain_trans")
tw <- Tweedie(p = 1.1)

# gaussian models
gam_g <- 
  as.list(meas) %>% 
  map(~model_pelvic_gam(resp = .x, fam = gaussian, data = c_data, method = "REML"))

# tweedie models
gam_t <- 
  as.list(meas) %>% 
  map(~model_pelvic_gam(resp = .x, fam = tw, data = c_data, method = "REML"))

# sets names
names(gam_g) <- paste0(meas, "_gaus")
names(gam_t) <- paste0(meas, "_tweedie")

# combines
gam_mods <- c(gam_g, gam_t)

# glance
gam_omni <- gam_mods %>% map(~glance_gam(.x)) %>% list_rbind(names_to = "model")

# estimates
gam_ests <- gam_mods %>% map(~tidy_gam(.x)) %>% list_rbind(names_to = "model")

# plotting AIC
pdata <- 
  as_tibble(gam_omni) %>% 
  mutate(family = if_else(grepl("gaus", model), "Gaussian", "Tweedie")) %>%
  mutate(meas = gsub("_gaus|_tweedie", "", model))
aic_plot <-
  ggplot(pdata, aes(family, AIC)) + 
    geom_bar(stat = "identity", width = .2, fill = "grey", color = "black") +
    labs(x = "Family") +
    theme_bw() +
    facet_wrap(~meas, scales = "fixed")
aic_plot

# QQPLOTS

## retrives residuals
resid_df <- 
  gam_mods %>% 
  map(~tibble(resid = residuals(.x))) %>% 
  list_rbind(names_to = "model")

## plots with ggplot2
qq_plot <- 
  ggplot(resid_df, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    x = "Theoretical Quantiles", 
    y = "Sample Quantiles", 
    title = "QQ plots of residuals."
    ) +
  theme_bw() +
  facet_wrap(~model)

# plotting QQ-plots using base R
# this doesn't save well with Rscript, so I used ggplot2 above instead
# keeping here because the code is cool
# par(mfrow = c(2,2), mar = c(4, 4, 2, 1))
# for (i in seq_along(gam_mods)) {
#   qq.gam(gam_mods[[i]], main = names(gam_mods)[i])
# }
# qq_plot <- recordPlot() # saves into object


# PREDICTIONS ----

## function to help generate + plot predictions
gen_plot_preds <- function(model, this_title){
  
  # generating new data
  mod_frame <- model.frame(model) # extracts modeling data
  new_data <- 
    with(
      mod_frame, 
      expand.grid(
        subject_id = "theoretical",
        yrs_since_baseline = c(0, 1, 2),
        PC1 = seq(min(PC1), max(PC1), length.out = 100),
        PC2 = mean(PC2),
        PC3 = mean(PC3)
      )
    )
  
  # generates predictions
  preds <- predict_link(model, newdat = new_data)
  
  # plots predictions
  p <- 
    ggplot(preds, aes(PC1, pred_resp)) +
    geom_ribbon(aes(ymin = lwr_resp, ymax = upr_resp), alpha = .25) +
    geom_line() +
    labs(
      x = "MMH (PC1)", 
      y = "Predicted Pelvic Pain",
      title = this_title,
      caption = "95% CI shading."
    ) +
    theme_bw() +
    facet_wrap(~yrs_since_baseline, labeller = label_both)
  
  # returns results as list
  res <- list(preds = preds, p = p)
  return(res)
}

# preallocates
gam_preds <- gam_mods # copies
titles <- 
  c(
    "Original Units + Gaussian Fit", 
    "Trans. Units + Gaussian Fit", 
    "Original Units + Tweedie Fit",
    "Trans. Units + Tweedie Fit"
    )

# generates + plots predictions for each model
for (i in seq_along(gam_mods)) {
  gam_preds[[i]] <- 
    gen_plot_preds(gam_mods$pelvic_pain_gaus, titles[i])
}

# SAVING RESULTS ----

# creates list
res <- 
  list(
    lm_1 = lm_1, # linear regression of PCs with menstrual pain at PV1
    mod1 = mod1, # mermod of PCs with menstrual pain across PV1 and PV2
    o_hist = o_hist, # histogram of pelvic pain original units
    t_hist = t_hist, # historgram of pelvic pain in transformed units
    gam_mods = gam_mods, # list of GAMs
    gam_omni = gam_omni, # df of omnibus stats from GAMs
    gam_ests = gam_ests, # df of estimates from GAMs
    aic_plot = aic_plot, # plot of the AICs between GAMs (to compare family)
    qq_plot = qq_plot, # qq plots of the models
    gam_preds = gam_preds # list of the predictions and plots for each GAM
  )
versioned_write_rds(data = res, vi = vinfo) # writes out



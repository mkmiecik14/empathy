# Analysis 2
# Matt Kmiecik
# 6 June 2022

# Purpose: this is analysis that examines the relationship between brain and
# behavior for the visual task. I will model the behavioral ratings as a function
# of brightness intensity and 25Hz power (SSVEP). This is is a simpler model
# than presented in Kmiecik et al., (2022), but useful for determining
# replication of findings

# Prepares workspace
source("r-prep.R")
source("topo_tools.R") # gets functions for plotting topos

# Loads data
load("../output/psd-res.rda") # spectral data
load("../output/visual-data-long.RData") # visual ratings data

# Prepping data - - - -
# SSVEP
hz_25_data <- 
  psd_res %>% 
  filter(freq == 25) %>% # narrows only to 25Hz 
  mutate(stim_mc = stim-3) # mean centers stim

# Behavioral data at baseline
vis_behav_baseline_data <- 
  visual_data_long %>% 
  filter(session == 10) # only baseline visit for join

# joins together
vis_data <- 
  left_join(hz_25_data, vis_behav_baseline_data, by = c("ss", "stim")) %>%
  group_by(ss, elec) %>%
  # mean centers psd
  mutate(psd_mc = as.numeric(scale(psd, center = TRUE, scale = FALSE))) %>% 
  ungroup()

length(unique(vis_data$ss)) #n

# Notes on missing data:
# ss252 has a missing block but keep
# ss356 and ss368 have missing e-prime...still waiting to find those data
missing_data <- vis_data %>% filter(is.na(rating))

# Linear mixed effects modeling - - - -
vis_mods <-
  vis_data %>%
  nest_by(elec) %>%
  mutate(
    min_mod = list(
      lmer(rating ~ 1 + stim_mc + psd_mc + (1 | ss), data = data, REML = TRUE)
    ),
    psd_mod = list(
      lmer(rating ~ 1 + stim_mc + psd_mc + (1 + psd_mc | ss), data = data, REML = TRUE)
    ),
    max_mod = list(
      lmer(rating ~ 1 + stim_mc + psd_mc + (1 + stim_mc + psd_mc | ss), data = data, REML = TRUE)
    )
  ) %>%
  mutate(anova_mod = list(anova(min_mod, max_mod))) # compares mods

#################
#               #
# Minimum Model #
#               #
#################

min_mod_ests <- vis_mods %>% summarise(broomExtra::tidy(min_mod)) %>% ungroup()

# cleans them up and inserts electrode locations
min_mod_ests_clean <-
  min_mod_ests %>% 
  filter(term %in% c("(Intercept)", "stim_mc", "psd_mc")) %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "stim_mc" ~ "brightness",
      term == "psd_mc" ~ "psd"
    ),
    sig = p.value < .05
  ) %>%
  group_by(term) %>%
  mutate(
    p.fdr = p.adjust(p.value, method = "fdr"),
    sig.fdr = p.fdr<.05
  ) %>%
  ungroup() %>%
  left_join(., elec_locs, by = c("elec" = "labels"))

# interpolates data for topo
min_mod_ests_clean_interp <- 
  min_mod_ests_clean %>%
  split(.$term) %>%
  map_dfr(~topo_interp(data = .x, dv = "estimate", gridRes = 67), .id = "term") %>%
  as_tibble(.)

# plots psd slopes
test <- min_mod_ests_clean %>% filter(term == "psd")
min(test$estimate); max(test$estimate)
topo_plot(
  orig_data = min_mod_ests_clean %>% filter(term == "psd"), 
  interp_data = min_mod_ests_clean_interp %>% filter(term == "psd"), 
  dv = estimate,
  legend_name = "PSD Slopes",
  electrode_size = 1.5,
  color_pal_limits = c(-.2, .2),
  color_pal_breaks = seq(-.2, .2, .1),
  bwidth = 2,
  color_pal = rev(brewer.pal(n = 9, "RdGy")),
  elec_shape_col = sig.fdr,
  elec_shapes = c(1, 16)
)

# plots stim slopes
test <- min_mod_ests_clean %>% filter(term == "brightness")
min(test$estimate); max(test$estimate)
topo_plot(
  orig_data = min_mod_ests_clean %>% filter(term == "brightness"), 
  interp_data = min_mod_ests_clean_interp %>% filter(term == "brightness"), 
  dv = estimate,
  legend_name = "Brightness Slopes",
  electrode_size = 1.5,
  color_pal_limits = c(-.8, .8),
  color_pal_breaks = seq(-.8, .8, .4),
  bwidth = 2,
  color_pal = rev(brewer.pal(n = 9, "RdGy")),
  elec_shape_col = sig.fdr,
  elec_shapes = c(16)
)

# looking at psd slopes - line graphs
min_mod_aug <- 
  vis_mods %>% 
  summarise(broom::augment(min_mod)) %>%
  ungroup()

this_data <- min_mod_aug %>% filter(elec==28, ss %in% c(1:75))
ggplot(this_data, aes(psd_mc, rating, group = ss)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = .5) +
  geom_smooth(
    aes(psd_mc, .fitted), 
    method = "lm", 
    se = FALSE, 
    linetype = 2, 
    color = "red",
    size = .5
    ) +
  labs(x = "PSD", y = "Unpleasantness", title = "Minimum Model") +
  facet_wrap(~ss) +
  theme_bw()
  

#################
#               #
# Maximum Model #
#               #
#################

max_mod_ests <- vis_mods %>% summarise(broomExtra::tidy(max_mod)) %>% ungroup()

# cleans them up and inserts electrode locations
max_mod_ests_clean <-
  max_mod_ests %>% 
  filter(term %in% c("(Intercept)", "stim_mc", "psd_mc")) %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "stim_mc" ~ "brightness",
      term == "psd_mc" ~ "psd"
    ),
    sig = p.value < .05
  ) %>%
  group_by(term) %>%
  mutate(
    p.fdr = p.adjust(p.value, method = "fdr"),
    sig.fdr = p.fdr<.05
  ) %>%
  ungroup() %>%
  left_join(., elec_locs, by = c("elec" = "labels"))

# interpolates data for topo
max_mod_ests_clean_interp <- 
  max_mod_ests_clean %>%
  split(.$term) %>%
  map_dfr(~topo_interp(data = .x, dv = "estimate", gridRes = 67), .id = "term") %>%
  as_tibble(.)

# plots intercepts
# All the same value because psd and stim were mean centered
test <- max_mod_ests_clean %>% filter(term == "Intercept")
min(test$estimate); max(test$estimate)
topo_plot(
  orig_data = max_mod_ests_clean %>% filter(term == "Intercept"), 
  interp_data = max_mod_ests_clean_interp %>% filter(term == "Intercept"), 
  dv = estimate,
  legend_name = "Power Spectral Density (dB)",
  electrode_size = 1.5,
  color_pal_limits = c(0, 20),
  color_pal_breaks = seq(0, 20, 5),
  bwidth = 2,
  elec_shape_col = sig.fdr,
  elec_shapes = c(16)
)

# plots stim slopes
test <- max_mod_ests_clean %>% filter(term == "brightness")
min(test$estimate); max(test$estimate)
topo_plot(
  orig_data = max_mod_ests_clean %>% filter(term == "brightness"), 
  interp_data = max_mod_ests_clean_interp %>% filter(term == "brightness"), 
  dv = estimate,
  legend_name = "Brightness Slopes",
  electrode_size = 1.5,
  color_pal_limits = c(-1, 1),
  color_pal_breaks = seq(-1, 1, .5),
  bwidth = 2,
  color_pal = rev(brewer.pal(n = 9, "RdGy")),
  elec_shape_col = sig.fdr,
  elec_shapes = c(16)
)

# plots psd slopes
test <- max_mod_ests_clean %>% filter(term == "psd")
min(test$estimate); max(test$estimate)
topo_plot(
  orig_data = max_mod_ests_clean %>% filter(term == "psd"), 
  interp_data = max_mod_ests_clean_interp %>% filter(term == "psd"), 
  dv = estimate,
  legend_name = "PSD Slopes",
  electrode_size = 1.5,
  color_pal_limits = c(-.2, .2),
  color_pal_breaks = seq(-.2, .2, .1),
  bwidth = 2,
  color_pal = rev(brewer.pal(n = 9, "RdGy")),
  elec_shape_col = sig.fdr,
  elec_shapes = c(1, 16)
)

# Looking at partial variance explained
# this is not straitforward in linear mixed models:
# https://stats.stackexchange.com/questions/358927/compute-partial-eta2-for-all-fixed-effects-anovas-from-a-lme4-model
# The marginal R squared (R2m) can be interpreted as the variance explained by 
# all fixed effects in the model, the conditional R squared (R2c) estimates the 
# variance explained by all fixed effects and all random effects in the model 
# taken together.
MuMIn::r.squaredGLMM(vis_mods$max_mod[[1]])
r2glmm::r2beta(vis_mods$max_mod[[1]], partial = TRUE)

partial_r2 <- 
  1:62 %>%
  map_dfr(~r2glmm::r2beta(vis_mods$max_mod[[.x]], partial = TRUE), .id = "elec") %>%
  as_tibble(.) %>%
  mutate(elec = as.numeric(elec)) %>%
  left_join(., elec_locs, by = c("elec" = "labels"))

ggplot(headShape, aes(x, y)) +
  geom_path() +
  geom_text(data = partial_r2 %>% filter(Effect == "psd_mc"), aes(x, y, label = round(Rsq*100, 2))) +
  geom_line(data = nose, aes(x, y, z = NULL)) +
  theme_topo() +
  coord_equal()

ggplot(headShape, aes(x, y)) +
  geom_path() +
  geom_point(data = partial_r2 %>% filter(Effect == "psd_mc"), aes(x, y, size = round(Rsq*100, 2))) +
  geom_line(data = nose, aes(x, y, z = NULL)) +
  theme_topo() +
  coord_equal()

partial_r2 %>% 
  split(.$Effect) %>%
  map(~topo_interp(data = .x, dv = "Rsq", gridRes = 67))# not working for some reason


# looking at psd slopes - line graphs
max_mod_aug <- 
  vis_mods %>% 
  summarise(broom::augment(max_mod)) %>%
  ungroup()

this_data <- max_mod_aug %>% filter(elec==28, ss %in% c(1:75))
ggplot(this_data, aes(psd_mc, rating, group = ss)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = .5) +
  geom_smooth(
    aes(psd_mc, .fitted), 
    method = "lm", 
    se = FALSE, 
    linetype = 2, 
    color = "red",
    size = .5
  ) +
  labs(x = "PSD", y = "Unpleasantness", title = "Maximum Model") +
  facet_wrap(~ss) +
  theme_bw()

# Several of the models did not converge; I would make sure that including the 
# random effects improves model fit
summary(vis_mods$max_mod[[7]])$optinfo$conv$lme4$messages

# Max models
1:62 %>% map(~summary(vis_mods$max_mod[[.x]])$optinfo$conv$lme4$messages)
# PSD models
1:62 %>% map(~summary(vis_mods$psd_mod[[.x]])$optinfo$conv$lme4$messages)
# Minimum models
1:62 %>% map(~summary(vis_mods$min_mod[[.x]])$optinfo$conv$lme4$messages)

####################
#                  #
# Model comparison #
#                  #
####################

# Max vs. Min models
mod_compare_ests <- 
  vis_mods %>% 
  summarise(broom::tidy(anova_mod)) %>%
  ungroup() %>%
  filter(term == "max_mod") %>%
  mutate(sig = p.value<.05) %>%
  left_join(., elec_locs, by = c("elec" = "labels"))

# interpolates data for topo
mod_compare_ests_interp <- 
  topo_interp(data = mod_compare_ests, dv = "statistic", gridRes = 67)

# plots stats
min(mod_compare_ests$statistic); max(mod_compare_ests$statistic)
topo_plot(
  orig_data = mod_compare_ests, 
  interp_data = mod_compare_ests_interp, 
  dv = statistic,
  legend_name = "Model Comparison \n (Chi-Square)",
  electrode_size = 1.5,
  color_pal_limits = c(50, 80),
  color_pal_breaks = seq(50, 80, 5),
  bwidth = 2,
  color_pal = brewer.pal(n = 9, "Blues"),
  elec_shape_col = sig,
  elec_shapes = c(16)
) 

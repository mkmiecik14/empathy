# Analysis 1
# Matt Kmiecik
# 3 June 2022

# Purpose: to replicate the proof of concept analysis performed in 
# Kmiecik et al. (2022) that demonstrates an increase in 25Hz SSVEP PSD as a
# function of increasing brightness intensity

# Prepares workspace
source("r-prep.R")
source("topo_tools.R") # gets functions for plotting topos

# Loads data
load("../output/psd-res.rda") # spectral data

# Linear mixed effect modeling (PSD ~ brightness intensity)
# Prepping data
hz_25_data <- 
  psd_res %>% 
  filter(freq == 25) %>% # narrows only to 25Hz 
  mutate(stim_mc = stim-3) # mean centers stim

# Modeling
hz_25_mod <-
  hz_25_data %>% 
  nest_by(elec) %>%
  mutate(
    min_mod = list(
      lmer(psd ~ 1 + stim_mc + (1 | ss), data = data, REML = TRUE)
      ),
    max_mod = list(
      lmer(psd ~ 1 + stim_mc + (1 + stim_mc | ss), data = data, REML = TRUE)
    )
    ) %>%
  mutate(anova_mod = list(anova(min_mod, max_mod))) # compares mods

# Extracting estimates
max_mod_ests <- hz_25_mod %>% summarise(broomExtra::tidy(max_mod)) %>% ungroup()

# cleans them up and inserts electrode locations
max_mod_ests_clean <-
  max_mod_ests %>% 
  filter(term %in% c("(Intercept)", "stim_mc")) %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "stim_mc" ~ "brightness"
    ),
    sig = p.value < .05
    ) %>%
  group_by(term) %>%
  mutate(
    p.fdr = p.adjust(p.value, method = "fdr"),
    sig.fdr = p.fdr>.05
    ) %>%
  ungroup() %>%
  left_join(., elec_locs, by = c("elec" = "labels"))

# interpolates data for topo
max_mod_ests_clean_interp <- 
  max_mod_ests_clean %>%
  split(.$term,) %>%
  map_dfr(~topo_interp(data = .x, dv = "estimate", gridRes = 67), .id = "term") %>%
  as_tibble(.)

# plots intercepts
test <- max_mod_ests_clean %>% filter(term == "Intercept")
min(test$estimate); max(test$estimate)
topo_plot(
  orig_data = max_mod_ests_clean %>% filter(term == "Intercept"), 
  interp_data = max_mod_ests_clean_interp %>% filter(term == "Intercept"), 
  dv = estimate,
  legend_name = "Power Spectral Density (dB)",
  electrode_size = 1,
  color_pal_limits = c(-30, -15),
  color_pal_breaks = seq(-30, -15, 5),
  bwidth = 2
) 

# plots slopes
test <- max_mod_ests_clean %>% filter(term == "brightness")
min(test$estimate); max(test$estimate)
topo_plot(
  orig_data = max_mod_ests_clean %>% filter(term == "brightness"), 
  interp_data = max_mod_ests_clean_interp %>% filter(term == "brightness"), 
  dv = estimate,
  legend_name = "Brightness Intensity Slope",
  electrode_size = 1,
  color_pal_limits = c(-1.5, 1.5),
  color_pal_breaks = seq(-1.5, 1.5, .5),
  bwidth = 2,
  color_pal = rev(brewer.pal(n = 11, "RdGy")),
  elec_shape_col = sig.fdr, # TRY TO GET THIS TO WORK
  elec_shapes = c(1, 2)
) 


summary(hz_25_mod$max_mod[[1]])
performance(hz_25_mod$max_mod[[1]])
check_model(hz_25_mod$max_mod[[1]])
anova(hz_25_mod$min_mod[[1]], hz_25_mod$max_mod[[1]]) # compares against model without moderators

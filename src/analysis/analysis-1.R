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

# Averaging electrodes 17, 47, 48, and 28 for V1 coverage
v1 <- c(17, 47, 48, 28)
# Averages electrodes together
v1_psd_ss <- 
  psd_res %>%
  filter(elec %in% v1) %>%
  filter(complete.cases(psd)) %>%
  group_by(ss, stim, freq) %>%
  summarise(m = mean(psd), n = n()) %>%
  ungroup()
# Averages the stim conditions together
v1_psd_ss_2 <-
  v1_psd_ss %>%
  group_by(ss, freq) %>%
  summarise(
    M = mean(m),
    N = n()
  ) %>%
  ungroup()
# Averages the participants
v1_psd_sum <- 
  v1_psd_ss_2 %>%
  group_by(freq) %>%
  summarise(
    m = mean(M),
    sd = sd(M),
    n = n(),
    sem = sd/sqrt(n),
    ll = quantile(M, .025),
    ul = quantile(M, .975)
  ) %>%
  ungroup()

v1_psd_plot <- 
  ggplot(v1_psd_sum, aes(freq, m)) +
  #geom_ribbon(aes(ymin = m+sem, ymax = m-sem), alpha = 1/3) +
  geom_vline(xintercept = 25, color = "red", linetype = 2, size = 1, alpha = 1/2) +
  geom_path(size = 1) +
  labs(
    x = "Frequency", 
    y = "Power Spectral Density (dB)", 
    title = "V1 Averaged Across Brightness Intensity"
    ) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5), minor_breaks = NULL) +
  scale_y_continuous(limits = c(-35, -5), minor_breaks = NULL) +
  theme_bw() 

v1_topo <-  
  ggplot(headShape, aes(x, y)) +
    geom_path() +
    geom_point(
      data = elec_locs %>% filter(labels %in% v1), 
      aes(x, y), 
      size = 2
      ) +
    geom_line(data = nose, aes(x, y, z = NULL)) +
    theme_topo() +
    coord_equal()

v1_psd_plot | v1_topo

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
    sig.fdr = p.fdr<.05
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
  electrode_size = 1.5,
  color_pal_limits = c(-30, -15),
  color_pal_breaks = seq(-30, -15, 5),
  bwidth = 2,
  elec_shape_col = sig.fdr,
  elec_shapes = c(16)
) 

# plots slopes
test <- max_mod_ests_clean %>% filter(term == "brightness")
min(test$estimate); max(test$estimate)
topo_plot(
  orig_data = max_mod_ests_clean %>% filter(term == "brightness"), 
  interp_data = max_mod_ests_clean_interp %>% filter(term == "brightness"), 
  dv = estimate,
  legend_name = "Brightness Intensity Slope",
  electrode_size = 1.5,
  color_pal_limits = c(-1.5, 1.5),
  color_pal_breaks = seq(-1.5, 1.5, .5),
  bwidth = 2,
  color_pal = rev(brewer.pal(n = 11, "RdGy")),
  elec_shape_col = sig.fdr,
  elec_shapes = c(1, 16)
) 


summary(hz_25_mod$max_mod[[1]])
performance(hz_25_mod$max_mod[[1]])
check_model(hz_25_mod$max_mod[[1]])
anova(hz_25_mod$min_mod[[1]], hz_25_mod$max_mod[[1]]) # compares against model without moderators


# Model comparison data
mod_compare_ests <- 
  hz_25_mod %>% 
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
  color_pal_limits = c(0, 60),
  color_pal_breaks = seq(0, 60, 10),
  bwidth = 2,
  color_pal = brewer.pal(n = 9, "Blues"),
  elec_shape_col = sig,
  elec_shapes = c(1, 16)
) 

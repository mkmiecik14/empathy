# Visual Task EEG Data Quality Analysis/Visualization
# Matt Kmiecik
# 29 APRIL 2022

#* Purpose: loads the preprocessed spectral data from MATLAB (see spectral-prepro.R)
#* and visualizes it to check the quality of the data

# Prepares workspace
source("r-prep.R")

# Loads data
load("../output/psd-res.rda") # spectral data
# load("../output/chan-locs.rda") # channel location data
# later load visual unpleasantness data when ready

ggplot(psd_res %>% filter(elec == 28), aes(freq, psd, group = ss)) +
  coord_cartesian(xlim = c(0, 30)) +
  geom_path() +
  facet_wrap(~stim)

# Looking at average broadband spectrum 
psd_res_sum <-
  psd_res %>%
  filter(complete.cases(psd)) %>% # filters out ss with no data for a block
  group_by(stim, elec, freq) %>%
  summarise(
    m = mean(psd),
    sd = sd(psd),
    n = n(),
    sem = sd/sqrt(n),
    ll = quantile(psd, .025, na.rm = TRUE),
    ul = quantile(psd, .975, na.rm = TRUE)
  ) %>%
  ungroup()

# Electrode 28
ggplot(psd_res_sum %>% filter(elec == 28), aes(freq, m)) +
  geom_ribbon(aes(ymin = ll, ymax = ul), fill = "grey") +
  geom_line(size = 1) + 
  coord_cartesian(xlim = c(0, 30)) +
  facet_wrap(~stim)

# Looking at a topo map of 25Hz power across all subjects
hz_25_data <- 
  psd_res_sum %>% 
  filter(freq==25) %>%
  left_join(., elec_locs, by = c("elec" = "labels"))

# interpolates data for topo
hz_25_data_interp <- 
  hz_25_data %>%
  split(.$stim) %>%
  map_dfr(~topo_interp(data = .x, dv = "m", gridRes = 67), .id = "stim") %>%
  as_tibble(.)

# Average 25Hz signal
topo_plot(
  orig_data = hz_25_data, 
  interp_data = hz_25_data_interp, 
  dv = m,
  legend_name = "Power Spectral Density (dB)",
  electrode_size = 1
  ) +
  facet_wrap(~stim, nrow = 1)


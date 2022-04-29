# Visual Task EEG Data Quality Analysis/Visualization
# Matt Kmiecik
# 29 APRIL 2022

#* Purpose: loads the preprocessed spectral data from MATLAB (see spectral-prepro.R)
#* and visualizes it to check the quality of the data

# Prepares workspace
source("r-prep.R")

# Loads data
load("../output/psd-res.rda") # spectral data
load("../output/chan-locs.rda") # channel location data
# later load visual unpleasantness data when ready

ggplot(psd_res %>% filter(elec == 28), aes(freq, psd, group = ss)) +
  coord_cartesian(xlim = c(0, 30)) +
  geom_path() +
  facet_wrap(~stim)



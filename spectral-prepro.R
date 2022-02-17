# Preprocessing script to organize the spectral data into R
# Matt Kmiecik
# Started 24 FEB 2020

source("r-prep.R") # preps R workspace 

# Loading various data ----

# Loading in spectral results from matlab (.mat)
spec_files <- list.files("../output/3-spec-res/", pattern = "*-vis-spec-res.mat")

# Loading in channel locations
chan_locs <- read_xlsx("../output/easycap_elec_positions.xlsx")

# Gathering subject numbers
subjs <- as.numeric(gsub("[^[:digit:].]", "", spec_files))
# to help join below / for specificity 
subject_joiner <- tibble(ss_i = 1:length(subjs), ss = subjs)

# Reading in and unpacking spectral results ----
spec_res <- 
  spec_files %>%
  map(~readMat(file.path("../output/3-spec-res/", .x))) %>%
  map("spec.res") %>%
  map(~as.matrix(.x))

# Pulling out various spectral results ----
# brightness intensities along z dim
stim_spectra  <- spec_res %>% map(1) # PSD
stim_freqs <- spec_res %>% map(2) # frequencies
freqs <- stim_freqs[[1]][,,1] # vector of frequencies

# Extracting and tidying spectral results ----
# Each participant has:
# 62 rows (electrodes) * 257 (freqencies) * 5 (brightness intensities)
psd_res <-
  stim_spectra %>%
  map_dfr(
    ~bind_rows(apply(.x, 3, as_tibble), .id = "stim") %>% 
      mutate(elec = rep(chan_locs$labels, 5)) %>%
      relocate(elec, .after = stim) %>%
      pivot_longer(c(-stim, -elec)) %>%
      mutate(name = rep(freqs, 5*length(chan_locs$labels))),
    .id = "ss_i"
      ) %>%
  mutate(ss_i = as.numeric(ss_i)) %>% # convert to number
  left_join(., subject_joiner, by = "ss_i") %>% # joins with subject numbers
  rename(freq = name, psd = value) %>% # renaming
  select(ss, stim, elec, freq, psd) # reordering + deselecting (ss_i)

# Saving out data ----
# Spectral results
save(psd_res, file = "../output/psd-res.rda")
write_csv(psd_res, file = "../output/psd-res.csv")

# channel locations
save(chan_locs, file = "../output/chan-locs.rda")
write_csv(chan_locs, file = "../output/chan-locs.csv")

# Cleans workspace ----
rm(
  chan_locs,
  psd_res,
  spec_res,
  stim_freqs,
  stim_spectra,
  subject_joiner,
  freqs,
  spec_files,
  subjs
)

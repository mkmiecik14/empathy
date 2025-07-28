# prepro-disc-eeg.R
# Matt Kmiecik
# Purpose: extract broadband power spectrum and peak alpha measures from
# discover EEG MATLAB objects

# libraries ----
library(R.matlab); library(tidyverse)

prepro_psd <- function(fpath){
  
  # defining frequency bands
  bw <- .1 # bin width of frequency bins (see freq_bins)
  delta <-  seq(1, 3.9, bw)
  theta <-  seq(4, 7.9, bw)
  alpha <-  seq(8, 12.9, bw)
  beta <-   seq(13, 30, bw)
  
  tmp <- R.matlab::readMat(fpath) # reads in subject data
  elec <- unlist(tmp$power[[1]]) # gets electrodes in order
  elec <- as.vector(gsub("'", "", elec)) # removes the single apostrophe; vector
  
  freq_bins <- as.vector(tmp$power[[3]]) # retrieves frequency bins
  
  psd <- as.matrix(tmp$power[[4]])
  rownames(psd) <- elec
  
  psd_tib <- 
    t(psd) %>% # transposes matrix 
    as_tibble() %>% # converts to tibble
    mutate(freq_bin = freq_bins, .before = "GND") %>% # inserts freq bins
    pivot_longer(cols = -freq_bin, names_to = "elec", values_to = "power") %>%
    mutate(
      band = case_when(
        freq_bin %in% delta ~ "delta",
        freq_bin %in% theta ~ "theta",
        freq_bin %in% alpha ~ "alpha",
        freq_bin %in% beta ~ "beta",
        .default = NA
      )
    ) %>%
    filter(!is.na(band)) # (OPTIONAL) to reduce data if needed
  # returns to user
  return(psd_tib)
}

# usage
res <- prepro_psd(fpath = "data/discoverEEG/power/sub-004_task-EC_power.mat")


# prepro the peak freq data
prepro_peak <- function(fpath){
  tmp <- readMat(fpath)
  local_max <- as.vector(tmp$peakfrequency[[1]])
  cog <- as.vector(tmp$peakfrequency[[2]])
  res <- as_tibble(cbind(local_max, cog))
  return(res)
}

# usage
res <- prepro_peak(fpath = "data/discoverEEG/power/sub-004_task-EC_peakfrequency.mat")


# R preparation script
# Matt Kmiecik
# Started 17 FEB 2022

# Purpose: to prepare the R workspace
# Any packages, functions, variables, etc. that are used across scripts are
# coded in here to reduce redundancy of code

# Libraries ----
library(tidyverse)
library(Hmisc)
library(R.matlab)
library(readxl)

# Plotting Tools ----
# use geom_flat_violin()
# see: https://neuroconscience.wordpress.com/2018/03/15/introducing-raincloud-plots/
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

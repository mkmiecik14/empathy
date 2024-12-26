# Preprocessing API data
# Matt Kmiecik
# 13 June 2022

# Purpose: preprocess and prepare the data coming out of the API calls

# Prepares workspace
source("r-prep.R")

# Loads data
load("../output/ss-masterlist.rda") # masterlist
load("../output/empathy-baseline-api.rda") # baseline visit
load("../output/empathy-visit1-api.rda") # visit 1
load("../output/empathy-visit2-api.rda") # visit 2

# Preprocessing goes here:
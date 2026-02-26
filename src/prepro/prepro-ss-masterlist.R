# EMPATHY IDs
# Matt Kmiecik
# Started 13 June 2022

# Purpose: to create a .rda and .csv of EMPATHY participant IDs and
# redcap record numbers

# Prepares workspace
source("r-prep.R")

# Loads in excel document
ss_masterlist <- 
  read_excel(path = "../data/empathy-ids.xlsx", sheet = "empathy-ids")

# Saves out masterlist
save(ss_masterlist, file = "../output/ss-masterlist.rda")
write_csv(ss_masterlist, file = "../output/ss-masterlist.csv")

# Removes script objects
rm(ss_masterlist)
# analysis-template-script.R
# Matt Kmiecik
# Started [ date ]
# Current version: [ v1 ]
# Purpose: [ description here ]

# grabs versioning info ----
source("src/fns/versioning_proc.R")
vinfo <- versioning_proc(testing = FALSE, this_script = "[script-name-here]")

# write analysis here

# saving ----
# versioned_write_rds(data = [DATA GOES HERE], vi = vinfo) # writes out
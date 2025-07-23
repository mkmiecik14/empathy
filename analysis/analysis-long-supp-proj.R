# analysis-long-supp-proj.R
# Matt Kmiecik
# Started 2025-07-22
# Current version: v1
# Purpose: compute MMH longitudinally via supplementary projections and examine
# change over time

# grabs versioning info ----
source("src/fns/versioning_proc.R")
vinfo <- 
  versioning_proc(testing = TRUE, this_script = "analysis-long-supp-proj")

# libraries ----
library(tidyverse); library(TInPosition)

# data ----

## PCA data
f <- file.path("output", "pca-data.rds")
dd <- read_rds(file = f)

# these cols need to be flipped so that increased scores == more sensitive
tcols <- c("knee_PPT", "shoulder_PPT", "knee_CPM", "shoulder_CPM")
dd_flip <- 
  dd %>% 
  mutate(across(.cols = all_of(tcols), .fns = ~-1*.x)) %>% # flips
  select(-shoulder_CPM) # shoulder CPM was dropped in PCA analysis 3
  
## Previous PCA models

# experimental only
f <- file.path("output", "analysis-pca-3-exp-res.rds")
exp_pca_res <- read_rds(file = f)

# experimental + questionnaires
f <- file.path("output", "analysis-pca-3-q-res.rds")
q_pca_res <- read_rds(file = f)


# analysis ----

# creates data sets
dd_visit12 <- dd_flip %>% filter(!grepl("baseline", redcap_event_name)) # not baseline
dd_visit12_c <- dd_visit12 %>% filter(complete.cases(.)) # complete data
dd_visit12_nc <- dd_visit12 %>% filter(!complete.cases(.)) # missing data

# formatting complete data for supplementary projection
supp_data <- 
  dd_visit12_c %>% 
  mutate(
    redcap_event_name = case_when(
      grepl("visit_1", redcap_event_name) ~ "v1",
      grepl("visit2", redcap_event_name) ~ "v2",
      .default = NA
      )
    ) %>%
  mutate(id = paste0(subject_id, "_", redcap_event_name), .before = subject_id)
supp_data <- as.data.frame(supp_data) # creates data frame
rownames(supp_data) <- supp_data$id # sets rownames to id + visit
supp_data <- as.matrix(supp_data[, -c(1:3)]) # creates matrix

# creating supplementary proj data for experimental only
q_cols <- c("cssi", "bodymap", "gss", "hsc_mean") # questionnaire columns
supp_data_e <- supp_data[, base::setdiff(colnames(supp_data), q_cols)]

# computing supplementary projections for each participant at each visit

# supplementary projections of experimental only
supp_res_exp <- 
  supplementaryRows(
    SUP.DATA = supp_data_e, 
    res = exp_pca_res$pca_res$Fixed.Data
    )

# supplementary projections of questionnaires
supp_res_q <- 
  supplementaryRows(SUP.DATA = supp_data, res = q_pca_res$Fixed.Data)

# plotting movement across components

## experimental
exp_fi <- exp_pca_res$pca_res$Fixed.Data$ExPosition.Data$fi # isolates fi data
exp_ss <- paste0(rownames(exp_fi), "_v0")
rownames(exp_fi) <- exp_ss
exp_fi <- rbind(exp_fi, supp_res_exp$fii)

exp_fi_tib <- 
  as_tibble(exp_fi, rownames = "id") %>% 
  pivot_longer(-id) %>% mutate(name = gsub("V", "PC", name))

pdata <- exp_fi_tib %>% filter(name %in% c("PC1", "PC2"))
# I changed my mind, start out with wide and select cols from there
ggplot(pdata, aes()) 

# saving ----
# versioned_write_rds(data = [DATA GOES HERE], vi = vinfo) # writes out
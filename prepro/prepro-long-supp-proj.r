# prepro-long-supp-proj.R
# Matt Kmiecik
# Purpose: prepare the data for the longitudinal linear mixed modeling of MMH 
# via supplementary projections

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

# proc ----

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

# combining observed FI with supplemental projections

# function that helps extract fi data and combine with supp projs
extract_fi <- function(pca_fi_data, supp_fi_data){
  fi <- pca_fi_data                 # isolates fi data
  ss <- paste0(rownames(fi), "_v0") # appends visit info to ss
  rownames(fi) <- ss                # updates rownames
  fi <- rbind(fi, supp_fi_data)     # rowbinds with supp projections
  # creates tibble
  fi_tib <- 
    as_tibble(fi, rownames = "id") %>% 
    separate(id, into = c("subject_id", "visit")) %>%
    mutate(visit = as.numeric(gsub("v", "", visit)))
  colnames(fi_tib) <- gsub("V", "PC", colnames(fi_tib))
  return(fi_tib)
}

# extracts fi data and combines with supplementary projections
exp_fi_tib <- 
  extract_fi(exp_pca_res$pca_res$Fixed.Data$ExPosition.Data$fi, supp_res_exp$fii)
q_fi_tib <- 
  extract_fi(q_pca_res$Fixed.Data$ExPosition.Data$fi, supp_res_q$fii)



# first step is to retrieve the timestamps from redcap data
f <- file.path("data", "EH17338EMPATHY_DATA_2024-11-07_1140_noPHI.csv")
rc_data_raw <- read_csv(file = f)
rc_data <- 
  rc_data_raw %>% 
  select(subject_id, redcap_event_name, todaysdate) %>% 
  filter(!is.na(subject_id)) %>%
  mutate(
    todaysdate = mdy(todaysdate), subject_id = as.character(subject_id)
  ) %>%
  mutate(baseline_date = min(todaysdate), .by = subject_id) %>%
  mutate(
    yrs_since_baseline = time_length(interval(baseline_date, todaysdate), "years"),
    visit = case_when(
      grepl("baseline", redcap_event_name) ~ 0,
      grepl("visit_1", redcap_event_name) ~ 1,
      grepl("visit2", redcap_event_name) ~ 2,
      .default = NA
    )
  )
# data set to use in join
rc_data_time <- 
  rc_data %>% 
  select(subject_id, visit, todaysdate, baseline_date, yrs_since_baseline)

# joins fis with time data
exp_fi_time <- 
  left_join(exp_fi_tib, rc_data_time, by = c("subject_id", "visit")) %>%
  mutate(pca = "exp") # for join
q_fi_time <- 
  left_join(q_fi_tib, rc_data_time, by = c("subject_id", "visit")) %>%
  mutate(pca = "q")

# brings in pain data and processes further
pain_data <- 
  read_rds("output/prepro-long-pain-v1.rds") %>%
  mutate(
    visit = case_when(
      grepl("visit_1", redcap_event_name) ~ 1, 
      grepl("visit2", redcap_event_name) ~ 2, 
      .default = NA
    ),
    subject_id = factor(subject_id)
  ) %>%
  # computes pelvic pain both original and transformed
  rowwise() %>%
  mutate(pelvic_pain = mean(c_across(matches("q2|q3|q4")))) %>%
  ungroup() %>%
  # work to calculate composite using inverse hyperbolic sine transformation
  # using asinh()
  mutate(
    across(.cols = matches("q2|q3|q4"), .fns = ~asinh(.x), .names = "{.col}_trans") 
  ) %>%
  rowwise() %>%
  mutate(pelvic_pain_trans = mean(c_across(ends_with("trans"))))

# selects data for join
pain_data_join <- 
  pain_data %>% 
  select(
    subject_id, visit, menst_pain = painrating_child_q1, pelvic_pain, 
    pelvic_pain_trans
  )

# TANNER STAGE ----
tstage <- 
  read_rds("output/tanner-body-gss-data.rds") %>%
  mutate(
    visit = case_when(
      grepl("baseline", redcap_event_name) ~ 0,
      grepl("visit_1", redcap_event_name) ~ 1, 
      grepl("visit2", redcap_event_name) ~ 2, 
      .default = NA
    ),
    subject_id = factor(subject_id)
  ) %>%
  select(subject_id, visit, starts_with("tanner"))
  

# stores results into list
res <- list(
  exp_pca = exp_fi_time,
  q_pca = q_fi_time,
  pain = pain_data_join,
  tanner = tstage
)

# saves out ----
f <- file.path("output", "pca-3-fi-supp-proj-mod-data.rds")
write_rds(res, f)

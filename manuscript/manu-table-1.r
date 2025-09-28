# manu-table-1.R
# Matt Kmiecik
# Purpose: generate Table 1 of the manuscript

# libraries ----
library(tidyverse); library(rlang)

# data ----

# LONGITUDINAL MMH MODELING
f <- file.path("output", "analysis-long-supp-proj-simplified-v1.rds")
df <- read_rds(file = f)
mod1_data <- as_tibble(model.frame(df$model_res$mod1_res$mod), rownames = "r")
mod1_data$visit <- df$raw_data[mod1_data$r,]$visit
long_ss <- unique(as.character(mod1_data$subject_id))

# PCA RESULTS
f <- file.path("output", "analysis-pca-3-exp-res.rds")
pca_res <- read_rds(f)

# CONVERTING SCALED PCA BACK TO ORIGINAL UNITS
X <- pca_res$pca_res$Fixed.Data$ExPosition.Data$X # scaled data
means <- attr(X, "scaled:center") # means
sds <- attr(X, "scaled:scale") # standard deviations

# converts back to original units
unscaled_data <- sweep(X, 2, sds, "*")
unscaled_data <- sweep(unscaled_data, 2, means, "+")

# plotting scaled data
X_tib <- as_tibble(X, rownames = "subject_id")
X_long <- X_tib %>% pivot_longer(cols = -subject_id)
pca_ss <- unique(X_tib$subject_id)

cat(sprintf("The sample size of longitudinal modeling is n=%s\n", length(long_ss)))
cat(sprintf("The sample size of PCA3 is n=%s\n", length(pca_ss)))

# DEMOGRAPHIC DATA ----
f <- file.path("output", "EH17338EMPATHY_DATA_2024-11-07_1140_noPHI-joined.csv")
demo_data <- read_csv(f)
id_key <- 
  demo_data %>% 
  select(record_id, subject_id, parent_subjectid) %>%
  distinct()

# procedure for determining race for each subject_id
race_data <- 
  demo_data %>% 
  filter(redcap_event_name == "baselinevisit_pare_arm_1") %>%
  select(record_id, starts_with("demo_child_race")) %>% 
  distinct() %>%
  pivot_longer(cols = -record_id) %>%
  filter(value == 1) %>% # endorsed race
  mutate(race = str_extract(name, "\\d")) %>%
  left_join(
    ., 
    id_key %>% select(record_id, subject_id) %>% filter(!is.na(subject_id)) %>% 
      distinct(), 
    by = "record_id"
    ) %>%
  select(subject_id, race) %>%
  # converts race numeric to string
  mutate(
    race = case_when(
      race == 1 ~ "Asian",
      race == 2 ~ "American Indian or Alaska Native",
      race == 3 ~ "Black or African American",
      race == 4 ~ "Native Hawaiian or Other Pacific Islander",
      race == 5 ~ "White or Caucasian",
      .default = NA_character_
    )
  )

# helper function for demographic percentages
get_demo_sum <- function(
    ss, data = race_data, count_var = race, type = "cat", na_rm = FALSE, 
    include_missing_info = TRUE) {
  
  # Input validation
  if (!type %in% c("cat", "cont")) {
    stop("type must be either 'cat' or 'cont'")
  }
  
  if (missing(count_var)) {
    stop("count_var must be specified")
  }
  
  # Convert subject_id to match ss class if needed
  if (class(data$subject_id)[1] != class(ss)[1]) {
    data$subject_id <- as(data$subject_id, class(ss)[1])
  }
  
  # Calculate intersections
  this_ss <- unique(data$subject_id)
  matching_ss <- intersect(this_ss, ss)
  missing_ss <- setdiff(ss, this_ss)
  
  # Optional: warn about missing subjects
  if (length(missing_ss) > 0) {
    warning(paste("Warning:", length(missing_ss), "subject IDs not found in data"))
  }
  
  # Filter data once
  filtered_data <- data %>% filter(subject_id %in% ss)
  
  if (type == "cat") {
    res <- filtered_data %>%
      count({{ count_var }}, .drop = FALSE) %>%  # .drop = FALSE keeps zero counts
      mutate(
        N = length(matching_ss), 
        perc = n / N * 100,
        valid_N = sum(n)  # N excluding missing values
      )
    
    # Add missing value row if any
    if (na_rm == FALSE || any(is.na(filtered_data %>% pull({{ count_var }})))) {
      res <- res %>%
        add_row(
          !!as_name(enquo(count_var)) := NA,
          n = sum(is.na(filtered_data %>% pull({{ count_var }}))),
          N = length(matching_ss),
          perc = sum(is.na(filtered_data %>% pull({{ count_var }}))) / length(matching_ss) * 100,
          valid_N = sum(!is.na(filtered_data %>% pull({{ count_var }})))
        )
    }
    
  } else if (type == "cont") {
    # Fixed: Create proper summary statistics
    res <- filtered_data %>%
      summarise(
        variable = as_name(enquo(count_var)),
        N = length(matching_ss),
        valid_N = sum(!is.na({{ count_var }})),
        missing_N = sum(is.na({{ count_var }})),
        mean = mean({{ count_var }}, na.rm = na_rm),
        sd = sd({{ count_var }}, na.rm = na_rm),
        sem = sd/sqrt(valid_N),
        median = median({{ count_var }}, na.rm = na_rm),
        min = min({{ count_var }}, na.rm = na_rm),
        max = max({{ count_var }}, na.rm = na_rm),
        q25 = quantile({{ count_var }}, 0.25, na.rm = na_rm),
        q75 = quantile({{ count_var }}, 0.75, na.rm = na_rm)
      )
  }
  
  # Add metadata if requested
  if (include_missing_info) {
    attr(res, "missing_subjects") <- missing_ss
    attr(res, "n_missing_subjects") <- length(missing_ss)
    attr(res, "total_requested") <- length(ss)
  }
  
  return(res)
}

# getting race for each data set
pca_race <- get_demo_sum(ss = pca_ss, data = race_data, count_var = race)
long_race <- get_demo_sum(ss = long_ss, data = race_data, count_var = race)
attributes(pca_race)
attributes(long_race)

# ETHNICITY
ethnicity_data <- 
  demo_data %>% 
  filter(redcap_event_name == "baselinevisit_pare_arm_1") %>%
  select(record_id, demo_child_ethnicity) %>% 
  distinct() %>%
  pivot_longer(cols = -record_id) %>%
  filter(!is.na(value)) %>%
  mutate(ethnicity = value) %>%
  left_join(
    ., 
    id_key %>% select(record_id, subject_id) %>% filter(!is.na(subject_id)) %>% 
      distinct(), 
    by = "record_id"
  ) %>%
  select(subject_id, ethnicity) %>%
  # converts ethnicity numeric to string
  mutate(
    ethnicity = case_when(
      ethnicity == 1 ~ "Hispanic or Latino",
      ethnicity == 2 ~ "Non-Hispanic or Latino",
      .default = NA_character_
    )
  )

pca_eth <- get_demo_sum(ss = pca_ss, data = ethnicity_data, count_var = ethnicity)
long_eth <- get_demo_sum(ss = long_ss, data = ethnicity_data, count_var = ethnicity)
lapply(list(pca_eth, long_eth), attributes) # to see missing

# INCOME
income_lvls <- 
  c("<$25k", "$25k - <$50k", "$50k - <$75k", "$75k - <$100k", "$100k - <$150k",
    "≥$150k", "Prefer not to answer")
income_data <- 
  demo_data %>%
  filter(redcap_event_name == "baselinevisit_pare_arm_1") %>%
  select(record_id, demo_income) %>% 
  distinct() %>%
  pivot_longer(cols = -record_id) %>%
  filter(!is.na(value)) %>%
  mutate(income = value) %>%
  left_join(
    ., 
    id_key %>% select(record_id, subject_id) %>% filter(!is.na(subject_id)) %>% 
      distinct(), 
    by = "record_id"
  ) %>%
  select(subject_id, income) %>% 
  distinct() %>%
  # converts income numeric to string
  mutate(
    income = case_when(
      income == 1 ~ income_lvls[1],
      income == 2 ~ income_lvls[2],
      income == 3 ~ income_lvls[3],
      income == 4 ~ income_lvls[4],
      income == 5 ~ income_lvls[5],
      income == 6 ~ income_lvls[6],
      income == 7 ~ income_lvls[7],
      .default = NA_character_
    ),
    income = factor(income, levels = income_lvls)
  ) 


pca_inc <- get_demo_sum(ss = pca_ss, data = income_data, count_var = income)
long_inc <- get_demo_sum(ss = long_ss, data = income_data, count_var = income)
lapply(list(pca_inc, long_inc), attributes) # to see missing

# AGE 
age_data <- 
  demo_data %>%
  filter(redcap_event_name == "baselinevisit_pare_arm_1") %>%
  select(record_id, demo_child_age) %>% 
  distinct() %>%
  pivot_longer(cols = -record_id) %>%
  filter(!is.na(value)) %>%
  mutate(age = value) %>%
  left_join(
    ., 
    id_key %>% select(record_id, subject_id) %>% filter(!is.na(subject_id)) %>% 
      distinct(), 
    by = "record_id"
  ) %>%
  select(subject_id, age) %>% 
  distinct()

pca_age <- get_demo_sum(pca_ss, age_data, age, "cont")
long_age <- get_demo_sum(long_ss, age_data, age, "cont")
lapply(list(pca_age, long_age), attributes) # to see missing

# TANNER STAGE
f <- file.path("output", "tanner-body-gss-data.rds")
tanner_data <- 
  read_rds(f) %>% 
  select(subject_id, redcap_event_name, starts_with("tanner"))

tanner_map <- 
  tanner_data %>% 
  pivot_longer(cols = -c(subject_id, redcap_event_name)) %>%
  filter(!is.na(value)) %>%
  split(interaction(.$redcap_event_name, .$name)) 

tanner_map_pca <- 
  tanner_map %>% map(~get_demo_sum(pca_ss, data = .x, value, "cont"))
tanner_map_long <- 
  tanner_map %>% map(~get_demo_sum(long_ss, data = .x, value, "cont"))

# helper function to organize the mapping 
# (you could incorporate the steps above and also retrive the missing ss)
org_tan_map <- function(map_res){
  res <- 
    map_res %>% 
    list_rbind(names_to = "r") %>% 
    separate(r, into = c("event", "meas"), sep = "\\.")
  return(res)
}

pca_tanner <- org_tan_map(tanner_map_pca)
long_tanner <- org_tan_map(tanner_map_long)

# Assembling table
# code goes here

# manu-bladder-task-imputation-info.R
# Matt Kmiecik
# Purpose: provides information for methods on bladder task imputation

message("=== BLADDER TASK IMPUTATION INFORMATION ===")
message("")

# libraries ----
suppressPackageStartupMessages({
  library(tidyverse, warn.conflicts = FALSE)
})

# data ----

# bladder task imputed data
f <- file.path("output", "bladder-task-imp-data.rds") 
imp_data <- read_rds(f)

# PCA baseline data (for participant numbers)
f <- file.path("output", "analysis-pca-3-exp-res.rds")
pca_res <- read_rds(f)
pca_ss <- rownames(pca_res$pca_res$Fixed.Data$ExPosition.Data$fi)

# Longitudinal modeling data (for participant numbers)
f <- file.path("output", "analysis-long-supp-proj-simplified-v1.rds")
long_res <- read_rds(file = f)
long_ss <- unique(as.character(long_res$model_res$mod1_res$mod$model$subject_id))

# proc ---

# complete data
imp_data_c <- imp_data %>% filter(!is.na(pain) | !is.na(urgency)) 

print_imputation_numbers <- function(data, subjects = NULL, decimals = 2, print = TRUE){
  if (is.null(subjects)) {
    data <- data
  } else{
    if (class(subjects) != class(data[["subject_id"]])) {
      # convert "subjects" to same class as "subject_id"
      target_class <- class(data[["subject_id"]])
      subjects <- switch(
        target_class,
        "character" = as.character(subjects),
        "numeric" = as.numeric(subjects),
        "integer" = as.integer(subjects),
        "factor" = as.factor(subjects),
        as(subjects, target_class)  # fallback
      )
    } else{
      # do nothinbg
    }
    data <- filter(data, subject_id %in% subjects)
  }
  
  # processes data
  res <- 
    data %>%
    summarise(
      pain_imp_perc = mean(pain_imp)*100, # percent of pain imputed
      urg_imp_perc = mean(urgency_imp)*100, # percent of urgency imputed
      n_subjs = length(unique(subject_id)), # n subjects 
      n_pts = n(), # n data points
      .by = redcap_event_name,
    ) %>%
    mutate(across(where(is.numeric), ~round(.x, decimals))) %>%
    # helps reorder for printing
    mutate(
      redcap_event_name = factor(
        redcap_event_name, 
        levels = c(
          "baseline_visit_chi_arm_1", "visit_1_child_arm_1", "visit2_child_arm_1"
          )
        )
      ) %>%
    arrange(redcap_event_name)
  
  # prints nicely to terminal or returns
  if (print) {
    print(knitr::kable(res, format = "simple"))
  } else{
    return(res)
  }
  
}

# Full sample that imputation was performed on
message(
  "Full sample that the imputation was performed on (n=", 
  length(unique(imp_data_c$subject_id)), 
  "):"
  )
print_imputation_numbers(imp_data_c)
message("")

message(
  "Baseline Experimental PCA sample (n=", 
  length(pca_ss), 
  "):"
)
print_imputation_numbers(imp_data_c, subjects = pca_ss)
message("")

message(
  "Longitudinal MMH modeling sample (n=", 
  length(long_ss), 
  "):"
)
print_imputation_numbers(imp_data_c, subjects = long_ss)
message("")

# computing range of these
imp_ranges <- 
  bind_rows(
    print_imputation_numbers(imp_data_c, decimals = 4, print = FALSE),
    print_imputation_numbers(imp_data_c, subjects = pca_ss, decimals = 4, print = FALSE),
    print_imputation_numbers(imp_data_c, subjects = long_ss, decimals = 4, print = FALSE),
  .id = "dataset"
  ) %>%
  summarise(across(contains("imp"), ~max(.x) - min(.x)), .by = redcap_event_name)

message("=== Ranges (max - min) of the percentages across data sets: ===")
knitr::kable(imp_ranges, format = "simple")
            
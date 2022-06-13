# API Configuration
# Matt Kmiecik
# Started: 13 June 2022

# Purpose: run this script to refresh Redcap annual data

# Loads packages ----
source("r-prep.R") # Prepares R workspace

# API setup ----
api_url <- "https://survey.northshore.org/api/" # Redcap URL

# Tokens ----
empathy_api_token <- 
  read_lines(
    file = "C:/Analysis/empathy/private/EH17-338-api-token.txt" # token
  )

# Pulling data from API - - - -

# Pulling EMPATHY baseline visit data
empathy_baseline_api <- 
  POST(
    url = api_url,
    body = list(
      token = empathy_api_token,
      content = "record",
      format = "csv",
      type = "eav", # for longitudinal output
      "events[0]" = "baseline_visit_chi_arm_1",
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "true", # reports timestamps etc.
      exportDataAccessGroups = "false",
      returnFormat = "csv"
    )
  ) %>% 
  content(as = "parsed")

# Saves out data 
save(empathy_baseline_api, file = "../output/empathy-baseline-api.rda")
write_csv(empathy_baseline_api, file = "../output/empathy-baseline-api.csv")

# Pulling EMPATHY visit 1 data
empathy_visit1_api <- 
  POST(
    url = api_url,
    body = list(
      token = empathy_api_token,
      content = "record",
      format = "csv",
      type = "eav", # for longitudinal output
      "events[0]" = "visit_1_child_arm_1",
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "true", # reports timestamps etc.
      exportDataAccessGroups = "false",
      returnFormat = "csv"
    )
  ) %>% 
  content(as = "parsed")

# Saves out data 
save(empathy_visit1_api, file = "../output/empathy-visit1-api.rda")
write_csv(empathy_visit1_api, file = "../output/empathy-visit1-api.csv")

# Pulling EMPATHY visit 2 data
empathy_visit2_api <- 
  POST(
    url = api_url,
    body = list(
      token = empathy_api_token,
      content = "record",
      format = "csv",
      type = "eav", # for longitudinal output
      "events[0]" = "visit2_child_arm_1",
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "true", # reports timestamps etc.
      exportDataAccessGroups = "false",
      returnFormat = "csv"
    )
  ) %>% 
  content(as = "parsed")

# Saves out data 
save(empathy_visit2_api, file = "../output/empathy-visit2-api.rda")
write_csv(empathy_visit2_api, file = "../output/empathy-visit2-api.csv")


# Remove script objects
rm(
  api_url, 
  empathy_api_token, 
  empathy_baseline_api,
  empathy_visit1_api,
  empathy_visit2_api
  )



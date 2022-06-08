# API Configuration
# Matt Kmiecik
# Started: 08 June 2022

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

# Pulling data from API

##########
#        #
# CRAMPP #
#        #
##########

# Pulling CRAMPP screen visit data - arm 1
arm1_screenvisit_api <- 
  POST(
    url = api_url,
    body = list(
      token = arm1_api_token,
      content = "record",
      format = "csv",
      type = "eav", # for longitudinal output
      "events[0]" = "screen_visit_arm_1",
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
save(arm1_screenvisit_api, file = "../output/arm1-screenvisit-api.rda")
write_csv(arm1_screenvisit_api, file = "../output/arm1-screenvisit-api.csv")


# Remove script objects
rm(
  api_url, 
  arm1_api_token, 
  arm2_api_token, 
  nsaid_api_token, 
  shortened_annual_api_token,
  arm1_avisit1_api,
  arm1_avisit2_api,
  arm1_avisit3_api,
  arm1_screenvisit_api,
  nsaid_screenvisit_api,
  nsaid_baselinevisit_api,
  nsaid_followupvisit_api
  )



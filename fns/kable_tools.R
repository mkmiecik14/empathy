# kable_tools.R
# Matt Kmiecik
# 2025-06-12
# Purpose: functions that help build nice PDF tables in quarto/RMarkdown

round_df <- function(df, sci_not_cols = NULL, digits = 2) {
  
  # checking if packages are installed
  pkgs <- c("dplyr")
  for (i in pkgs) {
    if (!requireNamespace(i, quietly = TRUE)) {
      stop(paste0("Package ", i," is required but not installed."))
    }
  }
  
  # package loading
  lapply(pkgs, require, character.only = TRUE)
  
  # rounds data
  if (is.null(sci_not_cols)) {
    df2 <- df %>% mutate(across(where(is.numeric), ~round(.x, digits)))
  } else {
    df2 <- 
      df %>% 
      mutate(
        across(
          where(is.numeric) & !any_of(sci_not_cols), ~round(.x, digits)
        )
      ) %>%
      mutate(across(any_of(sci_not_cols), ~format(.x, digits = 3)))
  }
  return(df2)
}

kable_pdf <- function(data, ..., cap = ""){
  
  # checking if packages are installed
  pkgs <- c("kableExtra")
  for (i in pkgs) {
    if (!requireNamespace(i, quietly = TRUE)) {
      stop(paste0("Package ", i," is required but not installed."))
    }
  }

  data_rounded <- round_df(df = data, ...)
  kable(
    data_rounded, 
    format = "latex", booktabs = TRUE, longtable = TRUE, linesep = "", 
    align = "l",
    caption = cap
  ) %>%
    kable_styling(
      position = "left", 
      latex_options = c("striped", "repeat_header"), 
      stripe_color = "gray!15"
    )
  
}

# tidy_gam.R
# Matt Kmiecik
# 2025-06-11
# v1
# Purpose: functions that emulate broom:: for GAMs

tidy_gam <- function(model = NULL, type = "parametric", ...){
  
  # checking if packages are installed
  pkgs <- c("mgcv", "dplyr", "tibble")
  for (i in pkgs) {
    if (!requireNamespace(i, quietly = TRUE)) {
      stop(paste0("Package ", i," is required but not installed."))
    }
  }
  
  # package loading
  lapply(pkgs, require, character.only = TRUE)
  
  # checks that arguments are set
  if (!type %in% c("parametric", "smooth")) {
    stop("type must be parametric or smooth!")
  } else{}
  
  if (type == "parametric") {
    r <- 
      summary(model)$p.table %>% 
      as_tibble(rownames = "term") %>%
      mutate(type = "parametric", .before = "term")
    colnames(r) <- c("type", "term", "estimate", "se", "t", "p")
  } else{
    r <- 
      summary(model)$s.table %>% 
      as_tibble(rownames = "term") %>%
      mutate(type = "smooth", .before = "term")
    colnames(r) <- c("type", "term", "edf", "ref.df", "F", "p")
  }
  return(r)
}


glance_gam <- function(mod) {
  
  # checking if packages are installed
  pkgs <- c("mgcv")
  for (i in pkgs) {
    if (!requireNamespace(i, quietly = TRUE)) {
      stop(paste0("Package ", i," is required but not installed."))
    }
  }
  
  # package loading
  lapply(pkgs, require, character.only = TRUE)
  
  s <- summary(mod)
  r <- 
    data.frame(
      r.sq = s$r.sq,
      dev.expl = s$dev.expl,
      AIC = AIC(mod),
      BIC = BIC(mod),
      nobs = nobs(mod),
      residual.df = s$residual.df
      )
  return(r)
}
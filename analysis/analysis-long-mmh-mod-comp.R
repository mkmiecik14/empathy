# analysis-long-mmh-mod-comp.R
# Matt Kmiecik
# Started 2025-07-10
# Current version: v1
# Purpose: use model comparison to determine best PCA to use

# grabs versioning info ----
source("src/fns/versioning_proc.R")
vinfo <- 
  versioning_proc(testing = TRUE, this_script = "analysis-long-mmh-mod-comp")

# libraries ----
library(tidyverse)

# custom functions ----
source("src/fns/predict_link.R")
source("src/fns/broom_gam.R")

# data ----

## longitudinal pain data
f <- file.path("output", "prepro-long-pain-v1.rds")
d <- 
  read_rds(f) %>%
  mutate(
    yrs_since_pv1 = time_length(
      interval(baselinedate, todaysdate), unit = "years"
    ),
    .before = painrating_child_q1
  ) %>%
  mutate(subject_id = factor(subject_id)) %>%
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

## PCA results

# data
exp_pca <- read_rds(file = "output/analysis-pca-3-exp-res.rds")
q_pca <- read_rds(file = "output/analysis-pca-3-q-res.rds")

# function to extract fi
extract_fi <- function(data, suffix = ""){
  fi <- 
    data %>%
    as_tibble(rownames = "subject_id") %>%
    mutate(subject_id = as.double(subject_id)) %>%
    mutate(subject_id = factor(subject_id))
  colnames(fi) <- gsub("V", "PC", colnames(fi)) # changes to PC for clarity
  # suffix could be added on here if wanted
  fi <- fi %>% rename_with(~paste0(.x, suffix), .cols = starts_with("PC"))
  return(fi)
}

# extracts fi data and appends suffix to distinguish
exp_pca_fi <- extract_fi(exp_pca$pca_res$Fixed.Data$ExPosition.Data$fi, "")
q_pca_fi <- extract_fi(q_pca$Fixed.Data$ExPosition.Data$fi, "")

## combines PCA and longitudinal pain data into two data sets depending on the
## pca
comb_data_exp <- full_join(d, exp_pca_fi, by = "subject_id") # experimental PCs
comb_data_q <- full_join(d, q_pca_fi, by = "subject_id") # questionnaire PCs

# modeling ----
library(mgcv)

# function for modeling
model_pelvic_gam <- 
  function(resp = "pelvic_pain", fam = gaussian(), data = NULL, ...){
    fixed <- "yrs_since_pv1*(PC1 + PC2 + PC3)"
    smooths <- "s(subject_id, bs = 're')"
    rhs <- c(fixed, smooths)
    formula_string <- paste(resp, "~", paste(rhs, collapse = " + "))
    gam_formula <- as.formula(formula_string)
    mod <- gam(gam_formula, data = data, family = fam, ...)
    return(mod)
  }

## menstrual pain; gaussian fit; both data sets
datasets <- list(comb_data_exp, comb_data_q) # data sets into list

mp_g <- vector("list", length = length(datasets)) # preallocates res
pp_g <- mp_g
pp_tw <- mp_g
ppt_g <- mp_g
ppt_tw <- mp_g

tw <- Tweedie(p = 1.1) # sets Tweedie distribution

# runs Gaussian models
for (i in seq_along(datasets)) {
  
  # menstrual pain - gaussian
  mp_g[[i]] <- 
    model_pelvic_gam(
      resp = "painrating_child_q1", 
      fam = gaussian(),
      data = datasets[[i]]
      )
  
  # pelvic pain (original units) - gaussian
  pp_g[[i]] <- 
    model_pelvic_gam(
      resp = "pelvic_pain", 
      fam = gaussian(),
      data = datasets[[i]]
    )
  
  # pelvic pain (transformed) - gaussian
  ppt_g[[i]] <- 
    model_pelvic_gam(
      resp = "pelvic_pain_trans", 
      fam = gaussian(),
      data = datasets[[i]]
    )
  
  # pelvic pain (original units) - tweedie
  pp_tw[[i]] <- 
    model_pelvic_gam(
      resp = "pelvic_pain", 
      fam = tw,
      data = datasets[[i]]
    )
  
  # pelvic pain (transformed) - tweedie
  ppt_tw[[i]] <- 
    model_pelvic_gam(
      resp = "pelvic_pain_trans", 
      fam = tw,
      data = datasets[[i]]
    )
  
}

# organizes all results into named list
res_list <- 
  list(mp_g = mp_g, pp_g = pp_g, pp_tw = pp_tw, ppt_g = ppt_g, ppt_tw = ppt_tw)

# names inner list by PC dataset
res_list <- 
  lapply(res_list, function(x) {
    names(x) <- c("exp", "q")
    x
    })

# getting omnibus stats
pc_types <- c("exp", "q")
omni <- vector("list", length = length(pc_types)) -> ests
for (i in seq_along(pc_types)) {
  
  # omnibus
  omni[[i]] <- 
    res_list %>% 
    map(pc_types[i]) %>% 
    map(~glance_gam(.x)) %>% 
    list_rbind(names_to = "model") %>%
    mutate(pca = pc_types[i])
  
  # estimates
  ests[[i]] <-
    res_list %>% 
    map(pc_types[i]) %>% 
    map(~tidy_gam(.x)) %>% 
    list_rbind(names_to = "model") %>%
    mutate(pca = pc_types[i])
}

# combines into df
omni <- list_rbind(omni)
ests <- list_rbind(ests)

# plotting ----
ggplot(omni, aes(model, AIC, group = pca, fill = pca)) +
  geom_bar(stat = "identity", width = .5, position = "dodge", color = "black") +
  labs(x = "Model") +
  theme_bw()

ggplot(omni, aes(model, r.sq, group = pca, fill = pca)) +
  geom_bar(stat = "identity", width = .5, position = "dodge", color = "black") +
  labs(x = "Model") +
  theme_bw()

ggplot(omni, aes(model, BIC, group = pca, fill = pca)) +
  geom_bar(stat = "identity", width = .5, position = "dodge", color = "black") +
  labs(x = "Model") +
  theme_bw()

ests %>% filter(!grepl("(Intercept)", term)) %>%
  ggplot(aes(estimate, term, group = pca, color = pca)) +
  geom_point() + 
  facet_wrap(~model)
  

# saving ----
# versioned_write_rds(data = [DATA GOES HERE], vi = vinfo) # writes out
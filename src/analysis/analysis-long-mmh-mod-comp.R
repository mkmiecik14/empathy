# analysis-long-mmh-mod-comp.R
# Matt Kmiecik
# Started 2025-07-10
# Current version: v1
# Purpose: use model comparison to determine best PCA to use

# grabs versioning info ----
source("src/fns/versioning_proc.R")
vinfo <- 
  versioning_proc(testing = FALSE, this_script = "analysis-long-mmh-mod-comp")

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
    mod <- gam(gam_formula, data = data, family = fam, method = "REML", ...)
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

# estimates models
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

# extracting omnibus stats and estimates
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
omni2 <- 
  list_rbind(omni) %>%
  mutate(
    model_exp = case_when(
      model == "mp_g" ~ "Menstrual Pain (G)",
      model == "pp_g" ~ "Pelvic Pain (G)",
      model == "pp_tw" ~ "Pelvic Pain (Tw)",
      model == "ppt_g" ~ "Pelvic Pain Trans. (G)",
      model == "ppt_tw" ~ "Pelvic Pain Trans. (Tw)"
      )
    )
ests2 <- 
  list_rbind(ests) %>%
  mutate(
    model_exp = case_when(
      model == "mp_g" ~ "Menstrual Pain (G)",
      model == "pp_g" ~ "Pelvic Pain (G)",
      model == "pp_tw" ~ "Pelvic Pain (Tw)",
      model == "ppt_g" ~ "Pelvic Pain Trans. (G)",
      model == "ppt_tw" ~ "Pelvic Pain Trans. (Tw)"
    )
  )

# plotting ----

# function to plot barplots for omnibus model stats
plot_mod_bar <- function(data, dv){
  cc <- RColorBrewer::brewer.pal(12, "Set3")
  tcols <- c(exp = cc[3], q = cc[4])
  p <-
    ggplot(omni2, aes(model_exp, {{ dv }}, group = pca, fill = pca)) +
    geom_bar(stat = "identity", width = .5, position = "dodge", color = "black") +
    labs(x = "Model", fill = "PCA") +
    scale_fill_manual(
      values = tcols, 
      labels = c("Experimental", "Experimental + Questionniares")
    ) +
    theme_bw()
    return(p)
}

# plots 
aic_plot <- plot_mod_bar(omni2, AIC)
bic_plot <- plot_mod_bar(omni2, BIC)
rsq_plot <- 
  plot_mod_bar(omni2, r.sq) + 
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = expression(R^2))

# saving ----
res <- 
  list(
    models = res_list, omni = omni2, ests = ests2, aic_plot = aic_plot,
    bic_plot = bic_plot, rsq_plot = rsq_plot 
    )
versioned_write_rds(data = res, vi = vinfo) # writes out
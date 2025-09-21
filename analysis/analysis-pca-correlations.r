# analysis-pca-correlations.R
# Matt Kmiecik
# Current version: v1
# Purpose: compute the correlations between the questionnaires and the PCs
# Depends on analysis-pca-3.R

# grabs versioning info ----
source("src/fns/versioning_proc.R")
vinfo <- 
  versioning_proc(
    testing = FALSE, 
    this_script = "analysis-pca-correlations"
  )

# libraries ----
library(tidyverse); library(correlation)
library(RColorBrewer)
RDGY <- brewer.pal(11, "RdGy")

# data ----
f <- file.path("output", "pca-data.rds")
pca_data <- read_rds(f)

f <- file.path("output", "analysis-pca-3-exp-res.rds")
pca_res <- read_rds(f)

# PROC ----

# preps q data
q_data <- 
  pca_data %>% 
  filter(redcap_event_name == "baseline_visit_chi_arm_1") %>%
  select(subject_id, cssi, bodymap, gss, hsc_mean) %>%
  mutate(subject_id = as.character(subject_id))

# preps subject-wise factor scores and adds q data
fis <- 
  pca_res$pca_res$Fixed.Data$ExPosition.Data$fi %>%
  as.data.frame() %>% # weird glitch when using as_tibble straight from matrix array
  as_tibble(rownames = "subject_id") %>%
  mutate(subject_id = gsub("X", "", subject_id)) %>%
  left_join(., q_data, by = "subject_id")
colnames(fis) <- sub("^V", "PC", colnames(fis)) 

# computes correlations
cor_res <- 
  fis %>%
  correlation(
    select = paste0("PC", 1:3), 
    select2 = c("cssi", "bodymap", "gss", "hsc_mean"),
    p_adjust = "fdr"
  )

# converts to tibble and creates column for sig coloring
cor_res_tib <- as_tibble(cor_res) %>% mutate(sig_fdr = p<.05)
#cor_res_tib %>% filter(Parameter1 == "PC2", Parameter2 == "cssi")

# plot
tcol2 <- c("TRUE" = RDGY[3], "FALSE" = RDGY[8])
p1 <- 
  ggplot(cor_res_tib, aes(r, Parameter2, color = sig_fdr)) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_point() +
    geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = .1) +
    theme_bw() +
    coord_cartesian(xlim= c(-1, 1)) +
    scale_color_manual(values = tcol2) +
    facet_wrap(~Parameter1)
#p1

# saving out results ----
res <- 
  list(
    q_data = q_data,
    fis = fis, 
    cor_res = cor_res,
    cor_res_tib = cor_res_tib,
    plots = list(p1 = p1)
  )

# saving ----
versioned_write_rds(data = res, vi = vinfo)

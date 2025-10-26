# manu-consort.R
# Matt Kmiecik
# Purpose: generate numbers for the manuscript's CONSORT diagram

message("===================================")
message("=== MANUSCRIPT CONSORT DIAGRAM ====")
message("===================================")
message("")

# libraries ----
library(tidyverse)

# data ----

# LONGITUDINAL MMH MODELING
f <- file.path("output", "analysis-long-supp-proj-simplified-v1.rds")
df <- read_rds(file = f)
mod1_data <- as_tibble(model.frame(df$model_res$mod1_res$mod), rownames = "r")
mod1_data$visit <- df$raw_data[mod1_data$r,]$visit
long_ss <- unique(as.character(mod1_data$subject_id))

visit_n <- mod1_data %>% count(visit) # sample sizes at each visit


# This plot helps explain why there are more PV1 than baseline
tps <- 
  mod1_data %>% 
  count(subject_id) %>% 
  rename(timepoints = n) %>%
  mutate(timepoints = factor(timepoints)) 
pdata <- mod1_data %>% left_join(., tps, join_by(subject_id))
plot1 <- 
  ggplot(
    pdata, aes(yrs_since_baseline, PC1, group = subject_id, color = timepoints)
    ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point() +
  geom_line(alpha = 1/2) +
  theme_classic()

# number of timpoints contributed
timepoints_n <- 
  tps %>% count(timepoints) %>% mutate(N = sum(n), Percent = n/N * 100)

# AVERAGE TIME AT EACH VISIT FOR LONGITUDINAL MODELING
avg_time <- 
  mod1_data %>% 
  summarise(
    m = mean(yrs_since_baseline), sd = sd(yrs_since_baseline), n = n(), 
    .by = visit
    )

# PCA RESULTS
f <- file.path("output", "analysis-pca-3-exp-res.rds")
pca_res <- read_rds(f)
pca_ss <- rownames(pca_res$pca_res$Fixed.Data$ExPosition.Data$X)

# PRINTS
message("NUMBERS FOR CONSORT DIAGRAM:")
message("----------------------------")
message("The number of participants in the baseline PCA: n=", length(pca_ss))
message("The number of participants at each timepoint in the mixed-models:")
knitr::kable(visit_n, format = "simple")
message("The number of timepoints contributed by participants in the mixed-models:")
knitr::kable(timepoints_n, format = "simple")

message("NUMBERS FOR RESULTS SECTION:")
message("----------------------------")
message("The total number of participants for longitudinal modeling was: n=", length(long_ss))
message("Average time since baseline (in years) for each visit: ")
knitr::kable(avg_time, format = "simple")



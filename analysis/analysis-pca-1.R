# analysis-pca-1.R
# Matt Kmiecik
# Started 27 April 2025

# Purpose: analyze senensory and questionnaire measures using PCA

# libraries ----
library(tidyverse); library(TInPosition); library(scales)

# notes on procedure of how I installed Exposition
# first, I git cloned https://github.com/derekbeaton/ExPosition1.git into Documents
# then:
# f <- list.dirs(path = "/Users/mattk/Documents/ExPosition1", recursive = FALSE)
# f <- f[!grepl("\\.git|Distatis|MExPosition", f)] # removes git directory
# for (i in f) {
#   devtools::install_local(i)
# }

# data ----
f <- file.path("output", "pca-data.rds")
dd <- read_rds(file = f)

# these cols need to be flipped so that increased scores == more sensitive
tcols <- c("knee_PPT", "shoulder_PPT", "knee_CPM", "shoulder_CPM")
dd_flip <- 
  dd %>% 
  mutate(across(.cols = all_of(tcols), .fns = ~-1*.x)) %>% # flips cols
  filter(complete.cases(.))

# only baseline participants for initial PCA
dd_flip_bs <- dd_flip %>% filter(grepl("baseline", redcap_event_name))

# preps a matrix for PCA
pca_data <- as.matrix(dd_flip_bs %>% select(-subject_id, -redcap_event_name))
rownames(pca_data) <- dd_flip_bs$subject_id
pca_data

# runs PCA and bootstrapping
res <- epPCA(pca_data)

# viz results
scree_data <- 
  tibble(
    eigs = res$ExPosition.Data$eigs, 
    perc = res$ExPosition.Data$t
    ) %>%
  mutate(comp = factor(1:nrow(.)))
ggplot(scree_data, aes(comp, perc)) +
  geom_point() +
  geom_path(aes(group = 1)) +
  labs(x = "Component", y = "Percentage of Variance Explained") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_bw()

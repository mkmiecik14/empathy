# analysis-pca-2.R
# Matt Kmiecik
# Started 30 May 2025

# Purpose: analyze sensory and questionnaire measures using PCA
# UPDATE FROM analysis-pca-1.R: averages knee and shoulder CPM 

# FLAGS ----
print_graphs <- FALSE # set to TRUE if you want PCA plots to be printed to
iters <- 2000 # sets the amount of bootstrap/permutation iterations for PCA
script <- "analysis-pca-2" # script for file prefix when saving

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

# averages knee_CPM and shoulder_CPM
dd$CPM <- (dd$knee_CPM + dd$shoulder_CPM) / 2

# these cols need to be flipped so that increased scores == more sensitive
tcols <- c("knee_PPT", "shoulder_PPT", "knee_CPM", "shoulder_CPM", "CPM")
dd_flip <- dd %>% mutate(across(.cols = all_of(tcols), .fns = ~-1*.x)) # flips

# only baseline participants for initial PCA
dd_flip_bs <- 
  dd_flip %>% 
  filter(grepl("baseline", redcap_event_name)) %>%
  select(-knee_CPM, -shoulder_CPM) # removes other CPM meas to only use avg

# examines the nature of the missing data
miss <- dd_flip_bs %>% filter(!complete.cases(.))
miss$n_na <- apply(miss, 1, function(x) sum(is.na(x)))
miss %>% filter(n_na < 2)
#miss %>% filter(n_na == 1) %>% View()
nrow(dd_flip_bs) - nrow(miss) 
ggplot(miss, aes(n_na)) + 
  geom_histogram(binwidth=1) + 
  scale_x_continuous(breaks = seq(1, 30, 1), minor_breaks = NULL)

# preps a matrix for PCA
tmp <- dd_flip_bs %>% filter(complete.cases(.)) # removes missing data
pca_data <- as.matrix(tmp %>% select(-subject_id, -redcap_event_name)) # matrix
rownames(pca_data) <- tmp$subject_id # rownames as subject ID
q <- c("cssi", "bodymap", "gss", "hsc_mean") # questionnaires
supp_data <- pca_data[,q] # creates supplementary data
pca_data_exp <- pca_data[, !colnames(pca_data) %in% q] # creates PCA data w/o q

# runs PCA and bootstrapping
set.seed(1218) # sets seed for reproducible results

## PCA without questionnaires
res <- 
  epPCA.inference.battery(
    DATA = pca_data_exp,
    graphs = print_graphs,
    test.iters = iters
    )
# supplementary projections of questionnaires
supp_res <- supplementaryCols(SUP.DATA = supp_data, res = res$Fixed.Data)

## PCA with questionnaires included
res_q <- 
  epPCA.inference.battery(
    DATA = pca_data,
    graphs = print_graphs,
    test.iters = iters
  )

# writes out results ----

## PCA results with questionnaires as supplemental projections
f <- file.path("output", paste0(script, "-exp-res.rds"))
pca_exp_res <- list(res, supp_res)
names(pca_exp_res) <- c("pca_res", "supp_proj")
write_rds(pca_exp_res, file = f)

## PCA results with questionnaires included
f <- file.path("output", paste0(script, "-q-res.rds"))
write_rds(res_q, file = f)

# analysis-supp-proj-inf.R
# Matt Kmiecik
# Purpose: develop method to bootstrap questionnaire data

# libraries ----
library(tidyverse); library(TInPosition)

# data ----
f <- file.path("output", "analysis-pca-3-exp-res.rds")
exp_pca <- read_rds(f)
f <- file.path("output", "analysis-pca-3-q-res.rds")
q_pca <- read_rds(f)

# EXPERIMENTAL PCA----
exp_data_raw <- exp_pca$pca_res$Fixed.Data$ExPosition.Data$X
exp_res <- exp_pca$pca_res$Fixed.Data # extracts PCA results

# QUESTIONNAIRE PCA ---- 

# CONVERTING SCALED PCA BACK TO ORIGINAL UNITS
X <- q_pca$Fixed.Data$ExPosition.Data$X # scaled data
means <- attr(X, "scaled:center") # means
sds <- attr(X, "scaled:scale") # standard deviations
unscaled_data <- sweep(X, 2, sds, "*")
unscaled_data <- sweep(unscaled_data, 2, means, "+")

qs <- c("cssi", "bodymap", "gss", "hsc_mean") # questionnaires
q_data_raw <- unscaled_data[,qs] # extracts questionniares only


# CHECK TO MAKE SURE DATA IS EQUIVALENT ----
check <- sum(rownames(q_data_raw) == rownames(exp_data_raw)) == nrow(exp_data_raw)
if (check) {
  cat("Rownames are identical...carry on!\n")
} else{
  stop("STOP! Data sets not equivalent...")
}


# BEFORE bootstrapping: Calculate full sample parameters for supplementary data
supp_data_original <- q_data_raw  # original unscaled questionnaire data
full_sample_center <- colMeans(supp_data_original, na.rm = TRUE)
full_sample_scale <- apply(supp_data_original, 2, sd, na.rm = TRUE)

# Store these for use in bootstrapping
fixed_res <- exp_res
iter <- 2000
# supp_fj_boot_array <- vector("list", length = iter)
supp_fj_boot_array <- array(data = NA, dim = c(ncol(supp_data_original), length(fixed_res$ExPosition.Data$eigs), iter))

# Now bootstrap
for(i in 1:iter){
  boot_indices <- sample(1:nrow(supp_data_original), replace=TRUE)
  boot_supp_data_raw <- supp_data_original[boot_indices, ]
  
  # Pass the FULL SAMPLE preprocessing parameters as numeric vectors
  boot_supp_proj <- supplementaryCols(
    SUP.DATA = boot_supp_data_raw,
    res = fixed_res,
    center = full_sample_center,  # vector of full sample means
    scale = full_sample_scale     # Vector of fulle sample sds
  )
  
  # Store boot_supp_proj$fjj for this iteration
  supp_fj_boot_array[,,i] <- boot_supp_proj$fjj
}

boot.ratio.test(supp_fj_boot_array)

# After loop, calculate BSR-like statistic
# BSR = original_loading / SD(bootstrap_distribution)
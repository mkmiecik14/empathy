# analysis-pca-1-data-transfer.R
# Matt Kmiecik
# 2025-05-28

# Purpose: save out the data that was used in analysis-pca-1.R

# libraries ----
library(tidyverse)

# loads data ----
f <- file.path("output", "analysis-pca-1-q-res.rds")
q_res <- read_rds(file = f)

# processing ----
X <- q_res$Fixed.Data$ExPosition.Data$X # scaled data
means <- attr(X, "scaled:center") # means
sds <- attr(X, "scaled:scale") # standard deviations

# converts back to original units
unscaled_data <- sweep(X, 2, sds, "*")
unscaled_data <- sweep(unscaled_data, 2, means, "+")

# converts to tibble
d <- as_tibble(unscaled_data, rownames = "subject_id")

# writes out ----
f <- file.path("output", "analysis-pca-1-data-transfer-q-res.csv")
write_csv(d, file = f)

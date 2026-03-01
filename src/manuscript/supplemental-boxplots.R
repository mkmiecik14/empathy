# supplemental-boxplots.R
# Matt Kmiecik
# Purpose: vizualize differences in high vs. low MMH on original columns

# LIBRARIES ====================================================================
library(tidyverse)
library(ghibli)

# FUNCTIONS ====================================================================
source("fns/save_figure.R")

# CONFIGURATION ================================================================
CONFIG <- list(
  major_font_size = 8,
  minor_font_size = 6,
  font_family = "Arial"
)

# DATA =========================================================================

# PCA RESULTS
f <- file.path("output", "analysis", "analysis-pca-3-exp-res.rds")
pca_res <- read_rds(f)

# WRANGLING ====================================================================

# getting quartiles of MMH
pc1_qtiles <-
  as_tibble(
    pca_res$pca_res$Fixed.Data$ExPosition.Data$fi,
    rownames = "subject_id"
) %>%
  select(subject_id, PC1 = V1) %>%
  mutate(
    qtile = ntile(PC1, n = 4),
    qtile = factor(
      qtile,
      levels = c(1:4)
    )
  )

# CONVERTING SCALED PCA BACK TO ORIGINAL UNITS
X <- pca_res$pca_res$Fixed.Data$ExPosition.Data$X # scaled data
means <- attr(X, "scaled:center") # means
sds <- attr(X, "scaled:scale") # standard deviations

# converts back to original units
unscaled_data <- sweep(X, 2, sds, "*")
unscaled_data <- sweep(unscaled_data, 2, means, "+")

# plotting scaled data
X_tib <-
  as_tibble(X, rownames = "subject_id") %>%
  left_join(., pc1_qtiles, by = join_by(subject_id))
X_long <-
  X_tib %>%
  pivot_longer(cols = -c(subject_id, PC1, qtile)) %>%
  mutate(name = case_when(
    name == "vis_mean"             ~ "Visual Task Mean",
    name == "vis_slope"            ~ "Visual Task Slope",
    name == "aud_mean"             ~ "Auditory Task Mean",
    name == "aud_slope"            ~ "Auditory Task Slope",
    name == "knee_PPT"             ~ "Knee PPT",
    name == "shoulder_PPT"         ~ "Shoulder PPT",
    name == "knee_CPM"             ~ "Knee CPM",
    name == "bt3a_baselinepain"    ~ "BT Baseline Pain",
    name == "bt7a_green_pain"      ~ "BT FS pain",
    name == "bt7b_yellowpain"      ~ "BT FU pain",
    name == "bt7c_redpain"         ~ "BT MT pain",
    name == "bt3_baselineurgency"  ~ "BT Baseline Urgency",
    name == "bt7a_greenurg"        ~ "BT FS Urgency",
    name == "bt7b_yellowurg"       ~ "BT FU Urgency",
    name == "bt7c_redurg"          ~ "BT MT Urgency",
    name == "cold_pain_10s"        ~ "Cold Pain 10s",
    name == "cold_pain_20s"        ~ "Cold Pain 20s",
    name == "afterpain_knee_1"     ~ "Knee After-Pain 1",
    name == "afterpain_shoulder_1" ~ "Shoulder After-Pain 1",
    name == "afterpain_knee_2"     ~ "Knee After-Pain 2",
    name == "afterpain_shoulder_2" ~ "Shoulder After-Pain 2",
    name == "afterpain_knee_3"     ~ "Knee After-Pain 3",
    name == "afterpain_shoulder_3" ~ "Shoulder After-Pain 3",
    name == "afterpain_hand"       ~ "Hand After-Pain",
    .default = name
  ))
pca_ss <- unique(X_tib$subject_id)

# PLOT =========================================================================

pal <- ghibli_palettes$LaputaMedium
tcols <- c("1" = pal[5], "4" = pal[6])
fig <-
  ggplot(X_long %>% filter(qtile %in% c(1, 4)), aes(qtile, value, fill = qtile)) +
  geom_hline(yintercept = 0, linetype = 2, alpha = .5, linewidth = .25) +
  geom_boxplot(
    width = 0.5, linewidth = 0.25, outlier.shape = 19, outlier.size = 0.5, 
    outlier.alpha = 0.4
  ) +
  labs(x = "MMH (PC1) Quartile", y = "Z-Score (SD units)") +
  theme_bw(base_size = CONFIG$major_font_size, base_family = CONFIG$font_family) +
  scale_fill_manual(values = tcols) +
  scale_x_discrete(labels = c("1" = "1 (Low)", "4" = "4 (High)")) +
  facet_wrap(~name, scales = "free") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_text(size = CONFIG$minor_font_size)
  )
fig

# SAVE =========================================================================

f <- file.path("output", "manuscript", "supplemental-boxplots.png")
save_figure(f = f, p = fig, w = 6, h = 5, units = "in", dpi = 300, both = TRUE)

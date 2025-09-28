# manu-pca-results.R
# Matt Kmiecik
# Purpose: generate the values needed for the textual Results of the PCA

# libraries ----
library(tidyverse); library(RColorBrewer); library(patchwork); library(ggrepel)

# configuration ----
CONFIG <- list(
  rdgy  = brewer.pal(11, "RdGy"),
  major_font_size = 10,
  minor_font_size = 8
)

# data ----
f <- file.path("output", "analysis-pca-3-exp-res.rds")
pca_res <- read_rds(f)

# PROC ----

# sample size of participants at baseline Experimental PCA
ss <- nrow(pca_res$pca_res$Fixed.Data$ExPosition.Data$X)
cat(
  sprintf(
    "Total sample size of participants at baseline with complete data for the Experimental PCA was n=%d",
    ss
    )
  )

########################
#                      #
# SCREE PLOT AND TABLE #
#                      #
########################

# assembling a table of components
scree_table <- 
  tibble(
    Eigenvalue = pca_res$pca_res$Inference.Data$components$eigs,
    Variance = pca_res$pca_res$Fixed.Data$ExPosition.Data$t,
    p = pca_res$pca_res$Inference.Data$components$p.vals
    ) %>%
  mutate(PC = 1:nrow(.), .before = Eigenvalue)
cat("Below is a scree plot table: \n")
print(scree_table)

# scree plot
scree_cols <- c("TRUE" = CONFIG$rdgy[3], "FALSE" = CONFIG$rdgy[8])
scree_plot <- 
  ggplot(scree_table, aes(PC, Variance, )) +
  geom_path(aes(group = 1), alpha = 1/3) +
  geom_point(aes(color = p < .05)) +
  labs(
    x = "Principal Component (PC)", 
    y = "% Variance Explained", 
    color = "Permutation Testing\n(2k iterations)"
    ) +
  scale_x_continuous(breaks = seq(1, nrow(scree_table), 1)) +
  scale_y_continuous(breaks = seq(0, 18, 2), labels = function(x) paste0(x, "%")) +
  coord_cartesian(ylim = c(0, 18)) +
  scale_color_manual(
    values = scree_cols, 
    labels = c(expression(italic(p) >= .05), expression(italic(p) < .05))
    ) +
  theme_classic(base_size = CONFIG$minor_font_size) +
  theme(
    legend.title = element_text(
      size = CONFIG$major_font_size-2,
      hjust = 0.5,                    # Center the title horizontally
      margin = margin(2, 2, 2, 2)     # Add some padding inside the box
    ),
    legend.title.position = "top",
    legend.background = element_rect(color = "black", linewidth = .25),
    legend.position = "inside",
    legend.position.inside = c(0.95, 0.95),    # x, y coordinates (0-1 scale)
    legend.justification = c(1, 1),     # Anchor point: right, top
    legend.text = element_text(size = CONFIG$minor_font_size-2)     # Legend labels
    )
scree_plot
  
# top 3 PCs
top_3_pcs <-  sum(pca_res$pca_res$Fixed.Data$ExPosition.Data$t[1:3])
cat(sprintf("The top three PCs explained %.2f%% of the variance.", round(top_3_pcs, 2)))

#######################
#                     #
# CORRELATION RESULTS #
#                     #
#######################

f <- file.path("output", "analysis-pca-correlations-v1.rds")
COR_res <- read_rds(f)
# write out some code that will print these results out nicely

####################
#                  #
# BOOTSTRAP RATIOS #
#                  #
####################

# critical value used to test bootstrap significance
crit_val <- pca_res$pca_res$Inference.Data$fj.boots$tests$critical.value

# extracts bootstrap ratios
bsrs <- 
  pca_res$pca_res$Inference.Data$fj.boots$tests$boot.ratios %>%
  as_tibble(rownames = "meas") %>%
  # convert names here
  mutate(meas = case_when(
    meas == "vis_mean" ~ "Visual Task mean",
    meas == "vis_slope" ~ "Visual Task slope",
    meas == "aud_mean" ~ "Auditory Task mean",
    meas == "aud_slope" ~ "Auditory Task slope",
    meas == "knee_PPT" ~ "Knee PPT",
    meas == "shoulder_PPT" ~ "Shoulder PPT",
    meas == "knee_CPM" ~ "Knee CPM",
    meas == "bt3a_baselinepain" ~ "BT Baseline pain",
    meas == "bt7a_green_pain" ~ "BT FS pain",
    meas == "bt7b_yellowpain" ~ "BT FU pain",
    meas == "bt7c_redpain" ~ "BT MT pain",
    meas == "bt3_baselineurgency" ~ "BT Baseline urgency",
    meas == "bt7a_greenurg" ~ "BT FS urgency",
    meas == "bt7b_yellowurg" ~ "BT FU urgency",
    meas == "bt7c_redurg" ~ "BT MT urgency",
    meas == "cold_pain_10s" ~ "Cold pain 10s",
    meas == "cold_pain_20s" ~ "Cold pain 20s",
    meas == "afterpain_knee_1" ~ "Knee after-pain 1",
    meas == "afterpain_shoulder_1"~ "Shoulder after-pain 1",
    meas == "afterpain_knee_2" ~ "Knee after-pain 2",
    meas == "afterpain_shoulder_2" ~ "Shoulder after-pain 2",
    meas == "afterpain_knee_3" ~ "Knee after-pain 3",
    meas == "afterpain_shoulder_3" ~ "Shoulder after-pain 3",
    meas == "afterpain_hand" ~ "Hand after-pain",
    .default = NA_character_
  ))

# to long format for plotting
bsrs_long <- 
  bsrs %>% 
  pivot_longer(cols = -meas) %>%
  mutate(sig = abs(value) > crit_val, name = gsub("V", "PC", name)) 

plot_bsrs <- function(data, pc = 1, ...){
  tcol <- c("TRUE" = CONFIG$rdgy[3], "FALSE" = CONFIG$rdgy[8])
  tlabels <- c("TRUE" = expression(italic(p) < .05), "FALSE" = expression(italic(p) >= .05))
  this_pc <- paste0("PC", pc)
  pdata <- data %>% filter(name == this_pc)
  p <-
    ggplot(pdata, aes(value, reorder(meas, value), fill = sig)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = c(-crit_val, crit_val), linetype = 2, color = CONFIG$rdgy[10]) +
    geom_vline(xintercept = 0, linetype = 1, color = "black") +
    scale_fill_manual(values = tcol, labels = tlabels) +
    labs(
      x = expression("Bootstrap Ratio (" * italic(t) * ")"), 
      y = "Measures", 
      title = this_pc,
      fill = "Bootstrapping\n(2k iterations)"
      ) +
    theme_bw(base_size = CONFIG$major_font_size) +
    theme(
      legend.title = element_text(
        hjust = 0.5,                    # Center the title horizontally
        margin = margin(5, 5, 5, 5)     # Add some padding inside the box
      ),
      legend.title.position = "top",
      legend.background = element_rect(color = "black"),
      legend.position = "inside",
      legend.position.inside = c(0.98, 0.15),    # x, y coordinates (0-1 scale)
      legend.justification = c(1, 1)     # Anchor point: right, top
      )
  return(p)
}

# helps align x-axis post hoc
bsrs_x_axis <- list(
  coord_cartesian(xlim = c(-8, 8)) , 
  scale_x_continuous(breaks = seq(-8, 8, 2))
)

# loops through and plots
bsrs_plots <- list()
for (i in 1:3) {
  bsrs_plots[[i]] <- plot_bsrs(data = bsrs_long, pc = i) + bsrs_x_axis
}

# wraps into one column using patchwork
bsrs_plots_wrapped <- wrap_plots(bsrs_plots, ncol = 1)

#######################################
#                                     #
# GEOMETRIC PLOTTING OF FACTOR SCORES #
#                                     #
#######################################

# long format of loadings
fj_long <- 
  pca_res$pca_res$Fixed.Data$ExPosition.Data$fj %>%
  as_tibble(rownames = "meas") %>%
  pivot_longer(cols = -meas) %>%
  mutate(name = gsub("V", "PC", name)) %>%
  left_join(., bsrs_long %>% select(meas, name, sig), by = c("meas", "name"))

# preps supplementary projection data
supp_proj <- 
  pca_res$supp_proj$fjj %>% 
  as_tibble(rownames = "meas") %>%
  pivot_longer(cols = -meas) %>%
  mutate(name = gsub("V", "PC", name))

# adds supp data to trained data
fj_long_supp <- bind_rows(fj_long, supp_proj)

# this is how I determined hard-coded min and max (axs_rng)
min_max <- list()
fj_values <- fj_long_supp %>% filter(name %in% paste0("PC", 1:3)) %>% pull(value)
min_max$min <- min(fj_values)
min_max$max <- max(fj_values)
nn <- pmax(abs(min_max$min), abs(min_max$max)) + 1 # create a square around this value

plot_fjs <- function(data, pcs = c(1,2), axs_rng = c(-12, 12), maxolaps = 10
){
  # plot elements
  meas_df <- 
    tribble(
      ~"meas", ~"cat",
      "vis_mean", "Visual",
      "vis_slope", "Visual",
      "aud_mean", "Auditory",
      "aud_slope", "Auditory",
      "knee_PPT", "PPT",
      "shoulder_PPT", "PPT",
      "knee_CPM", "CPM",
      "shoulder_CPM", "CPM",
      "CPM", "CPM",
      "bt3a_baselinepain", "Bladder",
      "bt7a_green_pain", "Bladder",
      "bt7b_yellowpain", "Bladder",
      "bt7c_redpain", "Bladder",
      "bt3_baselineurgency", "Bladder",
      "bt7a_greenurg", "Bladder",
      "bt7b_yellowurg", "Bladder",
      "bt7c_redurg", "Bladder",
      "cold_pain_10s", "Cold Pain",
      "cold_pain_20s", "Cold Pain",
      "afterpain_knee_1", "After-Pain",
      "afterpain_shoulder_1", "After-Pain",
      "afterpain_knee_2", "After-Pain",
      "afterpain_shoulder_2", "After-Pain",
      "afterpain_knee_3", "After-Pain",
      "afterpain_shoulder_3", "After-Pain",
      "afterpain_hand", "After-Pain",
      "cssi", "Supp. Proj. (Survey)",
      "bodymap", "Supp. Proj. (Survey)",
      "gss", "Supp. Proj. (Survey)",
      "hsc_mean", "Supp. Proj. (Survey)"
    )
  ppal <- brewer.pal(12, "Paired")
  meas_cols <- 
    c(
      "Visual" = ppal[1], "Auditory" = ppal[2], "PPT" = ppal[3], "CPM" = ppal[4],
      "Bladder" = ppal[5], "Cold Pain" = ppal[6], "After-Pain" = ppal[7], 
      "Supp. Proj. (Survey)" = ppal[8]
    )
  # preps data
  fj_long_plot <- 
    data %>% 
    filter(name %in% paste0("PC", pcs)) %>%
    pivot_wider(
      id_cols = meas, names_from = name, values_from = c(value, sig)
    ) %>%
    left_join(., meas_df, by = "meas") %>%
    # convert names here
    mutate(meas = case_when(
      meas == "vis_mean" ~ "Visual Task mean",
      meas == "vis_slope" ~ "Visual Task slope",
      meas == "aud_mean" ~ "Auditory Task mean",
      meas == "aud_slope" ~ "Auditory Task slope",
      meas == "knee_PPT" ~ "Knee PPT",
      meas == "shoulder_PPT" ~ "Shoulder PPT",
      meas == "knee_CPM" ~ "Knee CPM",
      meas == "bt3a_baselinepain" ~ "BT Baseline pain",
      meas == "bt7a_green_pain" ~ "BT FS pain",
      meas == "bt7b_yellowpain" ~ "BT FU pain",
      meas == "bt7c_redpain" ~ "BT MT pain",
      meas == "bt3_baselineurgency" ~ "BT Baseline urgency",
      meas == "bt7a_greenurg" ~ "BT FS urgency",
      meas == "bt7b_yellowurg" ~ "BT FU urgency",
      meas == "bt7c_redurg" ~ "BT MT urgency",
      meas == "cold_pain_10s" ~ "Cold pain 10s",
      meas == "cold_pain_20s" ~ "Cold pain 20s",
      meas == "afterpain_knee_1" ~ "Knee after-pain 1",
      meas == "afterpain_shoulder_1"~ "Shoulder after-pain 1",
      meas == "afterpain_knee_2" ~ "Knee after-pain 2",
      meas == "afterpain_shoulder_2" ~ "Shoulder after-pain 2",
      meas == "afterpain_knee_3" ~ "Knee after-pain 3",
      meas == "afterpain_shoulder_3" ~ "Shoulder after-pain 3",
      meas == "afterpain_hand" ~ "Hand after-pain",
      meas == "cssi" ~ "CSSI",
      meas == "bodymap" ~ "Body Map",
      meas == "gss" ~ "GSS",
      meas == "hsc_mean" ~ "HSC",
      .default = NA_character_
    ))
  # plot
  p <-
    ggplot(
      fj_long_plot, 
      aes(
        !!sym(paste0("value_PC", pcs[1])), !!sym(paste0("value_PC", pcs[2])), 
        color = cat
      )
    ) +
    geom_vline(xintercept = 0, linetype = 3, linewidth = .25) +
    geom_hline(yintercept = 0, linetype = 3, linewidth = .25) +
    geom_point(size = .25) +
    theme_minimal(base_size = CONFIG$major_font_size) +
    geom_text_repel(
      aes(label = meas), 
      show.legend = FALSE, 
      max.overlaps = maxolaps,
      family = "Arial",
      segment.size = .5,
      size = 1.5,
      segment.alpha = 0.25,
      force = 1,                     # Repulsion strength
      force_pull = 2,              # Attraction to original position
      seed = 42 # seed for reproducibility
      ) +
    coord_cartesian(xlim = axs_rng, ylim = axs_rng) +
    scale_color_manual(values = meas_cols) + 
    guides(
      color = guide_legend(
        title = "Categorized Measure", 
        override.aes = list(size = 1))
      ) +
    labs(x = paste0("PC", pcs[1]), y = paste0("PC", pcs[2])) +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),   # Remove x-axis tick labels
      axis.text.y = element_blank(),    # Remove y-axis tick labels
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      ## LEGEND
      legend.title = element_text(
        hjust = 0.5,                    # Center the title horizontally
        margin = margin(1, 1, 1, 1),     # Add some padding inside the box
        size = CONFIG$major_font_size - 2,
        ),
      legend.text = element_text(size = CONFIG$minor_font_size - 2),
      legend.key.size = unit(0.5, "cm"),        # Smaller legend symbols
      legend.title.position = "top",
      legend.background = element_rect(color = "black", linewidth = .25),
      legend.position = "inside",
      legend.position.inside = c(0.2, 0.35),    # x, y coordinates (0-1 scale)
      legend.justification = c(1, 1),     # Anchor point: right, top
      legend.box.spacing = unit(0.2, "cm")      # Less spacing
      )
  return(p)
} 

# helper function to plot the fjs
fjs_plot_wrapper <- function(data = fj_long_supp, pcs = c(1,2), n = nn, maxolaps = 20){
  p <- plot_fjs(data = data, pcs = pcs, axs_rng = c(-n, n), maxolaps = maxolaps)
  return(p)
}

# saves plots into list
fjs_plot_list <- list(
  fjs_12_plot = fjs_plot_wrapper(pcs = c(1,2)),
  fjs_23_plot = fjs_plot_wrapper(pcs = c(2,3)),
  fjs_13_plot = fjs_plot_wrapper(pcs = c(1,3))
)

# assembles figure 2
figure2 <- 
  wrap_plots(fjs_plot_list, ncol = 2) + guide_area() +
  plot_layout(guides = "collect", ncol = 2) +
  plot_annotation(tag_levels = c("A", "B", "C"), tag_suffix = ")") &
  theme(plot.tag = element_text(size = 10, face = "bold"))

##############################
#                            #
# SAVES FIGURES / TABLES OUT #
#                            #
##############################

# helper function for creating plots
save_figure <- function(f, p, w, h, units = "in", dpi = 600, both = TRUE) {
  
  # Validate inputs
  if (missing(f) || missing(p)) {
    stop("Both filename (f) and plot (p) must be provided")
  }
  
  # Extract file components
  base_name <- tools::file_path_sans_ext(f)
  file_ext <- tolower(tools::file_ext(f))
  
  # If extension provided, use base name; otherwise use full filename as base
  if (file_ext != "") {
    final_base <- base_name
  } else {
    final_base <- f
  }
  
  if (both) {
    # Create filenames for both formats
    png_file <- paste0(final_base, ".png")
    tiff_file <- paste0(final_base, ".tiff")
    
    # Save PNG
    ggsave(filename = png_file, plot = p, width = w, height = h, 
           units = units, dpi = dpi, device = "png")
    
    # Save TIFF with compression
    ggsave(filename = tiff_file, plot = p, width = w, height = h, 
           units = units, dpi = dpi, device = "tiff", compression = "lzw")
    
    cat("Files saved:\n", png_file, "\n", tiff_file, "\n")
    
  } else {
    # Save single file
    if (file_ext == "") {
      final_file <- paste0(f, ".png")  # Default to PNG
    } else {
      final_file <- f
    }
    
    ggsave(filename = final_file, plot = p, width = w, height = h, 
           units = units, dpi = dpi)
    
    cat("File saved:", final_file, "\n")
  }
}

# scree plot (supplemetal figure)
f <- file.path("output", "manuscript", "scree-plot.png")
save_figure(f, scree_plot, w = 3, h = 2.5)

# figure 2
f <- file.path("output", "manuscript", "figure-2.png")
save_figure(f, figure2, w = 5.5, h = 5, dpi = 300)

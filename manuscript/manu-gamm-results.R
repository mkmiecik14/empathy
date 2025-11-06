# manu-gamm-results.R
# Matt Kmiecik
# Purpose: generate figures / tables for the manuscript wrt GAMM results

message("================================")
message("=== MANUSCRIPT GAMM RESULTS ====")
message("================================")
message("")

# libraries ----
library(tidyverse); library(patchwork); library(RColorBrewer); library(ghibli)
library(grid); library(colorspace); library(ggsci); library(scales)

# Functions ----
source("src/fns/save_figure.r")
pts_to_mm <- function(pts) pts / 2.845

# CONFIGURATION ----
CONFIG <- list(
  rdgy  = brewer.pal(11, "RdGy"),
  major_font_size = 8,
  minor_font_size = 6,
  font_family = "Arial",
  font_color = "black",
  lw = 0.75, # line width
  lks = 0.3, # legend.key.size unit in cm
  legend_lw = .25
)

# plot elements for standardization
element_text_major <- 
  element_text(
    family = CONFIG$font_family, 
    color = CONFIG$font_color, 
    size = CONFIG$major_font_size
  )

element_text_minor <- 
  element_text(
    family = CONFIG$font_family, 
    color = CONFIG$font_color, 
    size = CONFIG$minor_font_size
  )

y_axis_standardized <- list(
  scale_y_continuous(breaks = seq(-8, 8, 2)),
    coord_cartesian(ylim = c(-8, 8))
)

# helper function for custom annotations
custom_annotate <- function(x, y, lab){
  geom_res <- 
  annotate(
    "label", x = x, y = y, label = lab, hjust = 0, 
    fill = CONFIG$rdgy[7], label.padding = unit(.5, "lines"),
    family = CONFIG$font_family, color = CONFIG$font_color, 
    size = pts_to_mm(CONFIG$minor_font_size), fontface = "italic"
  ) 
  return(geom_res)
}

# creates these in a list for a plot
cust_annots <- list(
  custom_annotate(0, 8, "Increased MMH"),
  custom_annotate(0, -8, "Decreased MMH")
)


# data ----

# LONGITUDINAL MMH MODELING
f <- file.path("output", "analysis-long-supp-proj-simplified-v1.rds")
df <- read_rds(file = f)

# recreating these figures for the manuscript
# df$plots$p3
# df$plots$p1
# df$plots$p2

# gets data that went into modeling
mod_data <- 
  data.frame(df$model_res$mod1_res$mod$model) %>% as_tibble(rownames = "r")

# line plot of supplementary proj. across time (i.e., df$plots$p3)
obs_mmh_plot <- 
  ggplot(mod_data, aes(yrs_since_baseline, PC1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(aes(group = subject_id), alpha = 1/3) +
  geom_smooth(
    method = "lm", se = TRUE, color = ghibli_palettes$MononokeMedium[5], alpha = 1/2, 
    fill = ghibli_palettes$MononokeMedium[5], linewidth = CONFIG$lw 
    ) +
  labs(
    x = "Years Since Baseline Visit", 
    y = "Observed MMH (PC1 Factor Scores)"
    ) +
  y_axis_standardized +
  theme_classic() +
  theme(
    axis.text.x = element_text_minor,
    axis.title.x = element_text_major,
    axis.text.y = element_text_minor,
    axis.title.y = element_text_major
  ) +
  cust_annots

# predicted MMH over time
pdata <- df$preds$time %>% as_tibble()
pred_mmh_time <- 
  ggplot(pdata, aes(yrs_since_baseline, pred_resp)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(
    aes(ymin = lwr_resp, ymax = upr_resp), 
    alpha = 1/3, fill = ghibli_palettes$MononokeMedium[3]
    ) +
  geom_line(color = ghibli_palettes$MononokeMedium[3], linewidth = CONFIG$lw) +
  labs(
    x = "Years Since Baseline Visit", 
    y = "Predicted MMH (PC1 Factor Score)"
    ) +
  y_axis_standardized +
  theme_classic() +
  theme(
    axis.text.x = element_text_minor,
    axis.title.x = element_text_major,
    axis.text.y = element_text_minor,
    axis.title.y = element_text_major
  )

# predicted MMH vs. pelvic pain
pdata2 <- df$preds$pelvic_pain %>% as_tibble()
max_pp <- max(pdata2$pelvic_pain_pv)
pred_mmh_pp <- 
  ggplot(pdata2, aes(pelvic_pain_pv, pred_resp)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(
    aes(ymin = lwr_resp, ymax = upr_resp), 
    alpha = 1/3, fill = ghibli_palettes$MononokeMedium[4]
  ) +
  geom_line(color = ghibli_palettes$MononokeMedium[4], linewidth = CONFIG$lw) +
  scale_x_continuous(breaks = seq(0, max_pp, 1)) +
  labs(
    x = "Pelvic Pain", 
    y = "Predicted MMH (PC1 Factor Score)"
  ) +
  y_axis_standardized +
  theme_classic() +
  theme(
    axis.text.x = element_text_minor,
    axis.title.x = element_text_major,
    axis.text.y = element_text_minor,
    axis.title.y = element_text_major
  )

# ASSEMBLING GAMM RESULTS PLOT

# plot list
plots <- list(obs_mmh_plot, pred_mmh_time, pred_mmh_pp)

# putting these together
fig <- 
  wrap_plots(plots, ncol = 3) + 
  plot_annotation(tag_levels = c("A", "B", "C"), tag_suffix = ")") &
  theme(
    plot.tag = element_text(
      size = CONFIG$major_font_size, 
      face = "bold", 
      family = CONFIG$font_family
    )
  )

# save out here
message("==================================")
message("=== SAVING OUT LONG MMH FIGURE ===")
message("==================================")
message("")
f <- file.path("output", "manuscript", "long-mmh-plot")
save_figure(f = f, p = fig, w = 6.5, h = 3, units = "in", dpi = 300, both = TRUE)

# PLOTTING QUARTILE PLOTS ---

# Observed quartiles for pelvic pain
pd <- position_dodge(width = .2)
pj <- position_jitter(width = .2)
pdata_ss <- 
  df$obs_quartiles_ss %>% 
  filter(name == "pelvic_pain_pv_q") %>%
  mutate(
    visit = factor(
      visit, 
      levels = c(0:2), 
      labels = c("Baseline", "PM Visit 1", "PM Visit 2")
      )
    )
pdata_sum <- 
  df$obs_quartiles_sum %>% 
  filter(name == "pelvic_pain_pv_q") %>%
  mutate(
    value = factor(value, levels = c("low", "middle", "high")),
    value_label = case_when(
      value == "low" ~ "Low (0-25%)",
      value == "middle" ~ "Middle (25-75%)",
      value == "high" ~ "High (75-100%)",
      .default = NA
      ),
    visit = factor(
      visit, 
      levels = c(0:2), 
      labels = c("Baseline", "PM Visit 1", "PM Visit 2")
    )
    )

# creating color palette
#my_color <- ghibli_palettes$MononokeMedium[4]  # Replace with your color
#colors <- lighten(my_color, amount = seq(0.6, 0, length.out = 3))
# uncomment to see colors:
# barplot(rep(1, length(colors)), col = colors, border = NA, axes = FALSE)
colors <- pal_jco("default")(3)
names(colors) <- c("low", "middle", "high")

# observed quartiles plot
label_lookup <- setNames(pdata_sum$value_label, pdata_sum$value)
obs_q_plot <- 
  ggplot(
    pdata_sum,
    aes(visit, m, group = value, color = value, fill = value)
    ) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 1/2, linewidth = .5) +
  # geom_point(
  #   data = pdata_ss, 
  #   aes(y = PC1), 
  #   shape = 16, alpha = 1/3, position = pj,
  #   size = .75
  #   ) +
  geom_point(position = pd) +
  geom_errorbar(
    aes(ymin = ll, ymax = ul), 
    width = .2, position = pd, linewidth = .5, linewidth = CONFIG$lw
    ) +
  geom_line(position = pd, linewidth = .5, linewidth = CONFIG$lw) +
  labs(
    x = "Visit", 
    y = "Observed MMH (PC1 Factor Score)", 
    fill = "Pelvic Pain\nQuartile Group",
    color = "Pelvic Pain\nQuartile Group"
    ) +
  coord_cartesian(ylim = c(-8, 8)) +
  scale_y_continuous(breaks = seq(-8, 8, 2)) +
  scale_color_manual(values = colors, labels = label_lookup) +
  scale_fill_manual(values = colors, labels = label_lookup) +
  theme_classic() +
  theme(
    axis.text.x = element_text_minor,
    axis.title.x = element_text_major,
    axis.text.y = element_text_minor,
    axis.title.y = element_text_major,
    # LEGEND =========================
    legend.position = "bottom",
    legend.background = element_rect(color = "black", linewidth = CONFIG$legend_lw),
    legend.box = "vertical",
    legend.box.just = "center",
    legend.title.position = "top",
    legend.title = element_text(
      hjust = .5, size = CONFIG$minor_font_size, family = CONFIG$font_family, 
      color = CONFIG$font_color
      ),
    legend.text = element_text_minor,
    legend.key.size = unit(CONFIG$lks, "cm")
  )

# PREDICTED QUARTILE BOUNDARIES

# sets names of this vector to use dynamically 
# these are wrong for some reason...go back to original script and fix
q_vec <- as_vector(df$quartile_preds$pelvic_pain$pelvic_pain_pv) %>% unique()
names(q_vec) <- names(df$quartile_boundaries$pelvic_pain[-1]) # drops 0% as it is the same as %25
#q_vec <- q_vec[names(q_vec) %in% c("25%", "75%", "100%")] # drops 50% to match other plot above
# colors <- lighten(my_color, amount = seq(0.8, 0, length.out = length(q_vec)))
#names(colors) <- names(q_vec)

# plot data
pdata <- 
  df$quartile_preds$pelvic_pain %>% 
  mutate(
    tile_group = cut(
      pelvic_pain_pv,
      breaks = c(-Inf, q_vec),  # Use -Inf instead of 0
      labels = c(names(q_vec)),
      include.lowest = TRUE
    ),
    tile_group_label = paste0(tile_group, " (", round(pelvic_pain_pv, 2), ")")
  ) %>%
  filter(tile_group != "50%") # drops 50% to match plot above

# constructs labels to group
label_lookup <- setNames(pdata$tile_group_label, pdata$tile_group)
names(colors) <- unique(pdata$tile_group)

# plot
pred_q_plot <- 
  ggplot(
  pdata %>% filter(tile_group != "50%"),
  aes(
    yrs_since_baseline, pred_resp, group = tile_group,
    color = tile_group, fill = tile_group
  )
) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 1/2, linewidth = .5) +
  geom_ribbon(aes(ymin = lwr_resp, ymax = upr_resp), alpha = 1/3, color = NA) +
  geom_line(linewidth = CONFIG$lw) +
  coord_cartesian(ylim = c(-8, 8)) +
  labs(
    x = "Years Since Baseline Visit",
    y = "Predicted MMH (PC1 Factor Score)",
    color = "Pelvic Pain\nQuartile Upper Boundary",
    fill = "Pelvic Pain\nQuartile Upper Boundary"
  ) +
  scale_color_manual(values = colors, labels = label_lookup) +
  scale_fill_manual(values = colors, labels = label_lookup) +
  scale_y_continuous(breaks = seq(-8, 8, 2)) +
  theme_classic() +
  theme(
    axis.text.x = element_text_minor,
    axis.title.x = element_text_major,
    axis.text.y = element_text_minor,
    axis.title.y = element_text_major,
    # LEGEND ========================================
    legend.position = "bottom",
    legend.background = element_rect(color = "black", linewidth = CONFIG$legend_lw),
    legend.box = "vertical",
    legend.box.just = "center",
    legend.title.position = "top",
    legend.text = element_text_minor,
    legend.title = element_text(
      hjust = .5, size = CONFIG$minor_font_size, family = CONFIG$font_family, 
      color = CONFIG$font_color
    ),
    legend.key.size = unit(CONFIG$lks, "cm")
  )



# assmebles
plots <- list(obs_q_plot, pred_q_plot)
fig <- 
  wrap_plots(plots, ncol = 2) + 
  plot_annotation(tag_levels = c("A", "B", "C"), tag_suffix = ")") &
  theme(
    plot.tag = element_text(
      size = CONFIG$major_font_size, 
      face = "bold", 
      family = CONFIG$font_family
    )
  )

message("==================================")
message("=== SAVING OUT QUARTILE PLOT =====")
message("==================================")
message("")

# saves
f <- file.path("output", "manuscript", "quartile-plot")
save_figure(f = f, p = fig, w = 6.5, h = 4, units = "in", dpi = 300, both = TRUE)


# TABLE OF GAMM RESULTS ----
p_table <- 
  df$model_res$mod1_res$est_p %>% 
  mutate(lwr = estimate - 1.96 * se, upr = estimate + 1.96 * se) %>%
  mutate(
    term = case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "tanner_breast" ~ "Tanner Stage (Breast)",
    term == "tanner_hair" ~ "Tanner Stage (Hair)",
    term == "yrs_since_baseline" ~ "Time",
    term == "menst_pain_pv" ~ "Menstrual Pain",
    term == "pelvic_pain_pv" ~ "Pelvic Pain",
    term == "yrs_since_baseline:menst_pain_pv" ~ "Time * Menstrual Pain",
    term == "yrs_since_baseline:pelvic_pain_pv" ~ "Time * Pelvic Pain",
    term == "menst_pain_pv:pelvic_pain_pv" ~ "Menstrual * Pelvic Pain",
    term == "yrs_since_baseline:menst_pain_pv:pelvic_pain_pv" ~ "Time * Menstrual * Pelvic Pain",
    .default = NA_character_
  ),
  type = "Parametric") %>%
  select(Term = term, b = estimate, LL = lwr, UL = upr, SE = se, t, p)

# saves out
message("=============================")
message("=== SAVING OUT GAMM TABLE ===")
message("=============================")
message("")
f <- file.path("output", "manuscript", "gamm-p-terms-table.csv")
write_csv(x = p_table, file = f)

# write out a cat() of R2 and AIC values for both models:

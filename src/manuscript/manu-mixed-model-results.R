# manu-mixed-model-results.R
# Matt Kmiecik
# Purpose: generate figures / tables for the manuscript wrt linear mixed model results

message("=========================================")
message("=== MANUSCRIPT MIXED MODEL RESULTS ====")
message("=========================================")
message("")

# libraries ----
library(tidyverse); library(patchwork); library(RColorBrewer); library(ghibli)
library(grid); library(colorspace); library(ggsci); library(scales); library(flextable)
library(kableExtra); library(officer); library(lme4); library(lmerTest)

# Functions ----
source("fns/save_figure.R")
pts_to_mm <- function(pts) pts / 2.845

# CONFIGURATION ----
CONFIG <- list(
  rdgy  = brewer.pal(11, "RdGy"),
  major_font_size = 8,
  minor_font_size = 6,
  font_family = "Arial",
  font_color = "black",
  lw = 0.75,
  lks = 0.3,
  legend_lw = .25
)

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

custom_annotate <- function(x, y, lab) {
  annotate(
    "label", x = x, y = y, label = lab, hjust = 0,
    fill = CONFIG$rdgy[7], label.padding = unit(.5, "lines"),
    family = CONFIG$font_family, color = CONFIG$font_color,
    size = pts_to_mm(CONFIG$minor_font_size), fontface = "italic",
    alpha = 0.6
  )
}

cust_annots <- list(
  custom_annotate(0, 7.8, "Increased MMH"),
  custom_annotate(0, -7.8, "Decreased MMH")
)

# data ----
f <- file.path("output", "analysis", "analysis-mixed-models.rds")
res <- read_rds(file = f)

#mod_data  <- res$raw_data
mod_data <- model.frame(res$models$fit2) %>% as_tibble()
fit2      <- res$models$fit2

# MAIN EFFECTS PLOTS ===========================================================

# line plot of raw trajectories
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
    y = "Observed MMH\n(PC1 Factor Scores)"
  ) +
  y_axis_standardized +
  theme_classic() +
  theme(
    axis.text.x  = element_text_minor,
    axis.title.x = element_text_major,
    axis.text.y  = element_text_minor,
    axis.title.y = element_text_major
  ) +
  cust_annots

# predicted MMH over time (pred1)
pdata <- res$preds$time
pred_mmh_time <-
  ggplot(pdata, aes(yrs_since_baseline, pred)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr),
    alpha = 1/3, fill = ghibli_palettes$MononokeMedium[3]
  ) +
  geom_line(color = ghibli_palettes$MononokeMedium[3], linewidth = CONFIG$lw) +
  labs(
    x = "Years Since Baseline Visit",
    y = "Predicted MMH\n(PC1 Factor Score)"
  ) +
  y_axis_standardized +
  theme_classic() +
  theme(
    axis.text.x  = element_text_minor,
    axis.title.x = element_text_major,
    axis.text.y  = element_text_minor,
    axis.title.y = element_text_major
  )

# predicted MMH vs. pelvic pain (pred2)
pdata2 <- res$preds$pelvic_pain
max_pp <- max(pdata2$pelvic_pain_pv)
pred_mmh_pp <-
  ggplot(pdata2, aes(pelvic_pain_pv, pred)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr),
    alpha = 1/3, fill = ghibli_palettes$MononokeMedium[4]
  ) +
  geom_line(color = ghibli_palettes$MononokeMedium[4], linewidth = CONFIG$lw) +
  scale_x_continuous(breaks = seq(0, max_pp, 1)) +
  labs(
    x = "Pelvic Pain",
    y = "Predicted MMH\n(PC1 Factor Score)"
  ) +
  y_axis_standardized +
  theme_classic() +
  theme(
    axis.text.x  = element_text_minor,
    axis.title.x = element_text_major,
    axis.text.y  = element_text_minor,
    axis.title.y = element_text_major
  )

# predicted MMH vs. menstrual pain (pred3, already generated in analysis script)
pdata3 <- res$preds$menst_pain
menst_range <- with(mod_data, data.frame(min = min(menst_pain_pv), max = max(menst_pain_pv)))
pred_menst_pp <-
  ggplot(pdata3, aes(menst_pain_pv, pred)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr),
    alpha = 1/3, fill = ghibli_palettes$MononokeMedium[6]
  ) +
  geom_line(color = ghibli_palettes$MononokeMedium[6], linewidth = CONFIG$lw) +
  scale_x_continuous(breaks = seq(menst_range$min, menst_range$max, 1)) +
  labs(
    x = "Menstrual Pain",
    y = "Predicted MMH\n(PC1 Factor Score)"
  ) +
  y_axis_standardized +
  theme_classic() +
  theme(
    axis.text.x  = element_text_minor,
    axis.title.x = element_text_major,
    axis.text.y  = element_text_minor,
    axis.title.y = element_text_major
  )

# ASSEMBLING MAIN EFFECTS FIGURE ===============================================
plots <- list(obs_mmh_plot, pred_mmh_time, pred_mmh_pp, pred_menst_pp)

fig <-
  wrap_plots(plots, ncol = 2) +
  plot_annotation(tag_levels = c("A", "B", "C", "D"), tag_suffix = ")") &
  theme(
    plot.tag = element_text(
      size = CONFIG$major_font_size,
      face = "bold",
      family = CONFIG$font_family
    )
  )

message("==================================")
message("=== SAVING OUT LONG MMH FIGURE ===")
message("==================================")
message("")
f <- file.path("output", "manuscript", "lmm-long-mmh-plot")
save_figure(f = f, p = fig, w = 5.5, h = 5, units = "in", dpi = 300, both = TRUE)

# PLOTTING QUARTILE PLOTS ======================================================

pd <- position_dodge(width = .2)
pdata_ss <-
  res$obs_quartiles_ss %>%
  filter(name == "pelvic_pain_pv_q") %>%
  mutate(
    visit = factor(
      visit,
      levels = c(0:2),
      labels = c("Baseline", "PM Visit 1", "PM Visit 2")
    )
  )

pdata_sum <-
  res$obs_quartiles_sum %>%
  filter(name == "pelvic_pain_pv_q") %>%
  mutate(
    value = factor(value, levels = c("low", "middle", "high")),
    value_label = case_when(
      value == "low"    ~ "Low (0-25%)",
      value == "middle" ~ "Middle (25-75%)",
      value == "high"   ~ "High (75-100%)",
      .default = NA
    ),
    visit = factor(
      visit,
      levels = c(0:2),
      labels = c("Baseline", "PM Visit 1", "PM Visit 2")
    )
  )

colors <- pal_jco("default")(3)[c(1, 3, 2)]
names(colors) <- c("low", "middle", "high")
y_scale <- scale_y_continuous(breaks = seq(-5, 5, 1))
y_coord <- coord_cartesian(ylim = c(-5, 5))

label_lookup <- setNames(pdata_sum$value_label, pdata_sum$value)
obs_q_plot <-
  ggplot(
    pdata_sum,
    aes(visit, m, group = value, color = value, fill = value)
  ) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 1/2, linewidth = .5) +
  geom_point(position = pd) +
  geom_errorbar(
    aes(ymin = ll, ymax = ul),
    width = .2, position = pd, linewidth = CONFIG$lw
  ) +
  geom_line(position = pd, linewidth = CONFIG$lw) +
  labs(
    x = "Visit",
    y = "Observed MMH\n(PC1 Factor Score)",
    fill  = "Pelvic Pain\nQuartile Group",
    color = "Pelvic Pain\nQuartile Group"
  ) +
  y_scale + y_coord +
  scale_color_manual(values = colors, labels = label_lookup) +
  scale_fill_manual(values = colors, labels = label_lookup) +
  theme_classic() +
  theme(
    axis.text.x  = element_text_minor,
    axis.title.x = element_text_major,
    axis.text.y  = element_text_minor,
    axis.title.y = element_text_major,
    legend.position        = "bottom",
    legend.background      = element_rect(color = "black", linewidth = CONFIG$legend_lw),
    legend.box             = "vertical",
    legend.box.just        = "center",
    legend.title.position  = "top",
    legend.title = element_text(
      hjust = .5, size = CONFIG$minor_font_size, family = CONFIG$font_family,
      color = CONFIG$font_color, face = "bold"
    ),
    legend.text     = element_text_minor,
    legend.key.size = unit(CONFIG$lks, "cm")
  )

# PREDICTED QUARTILE BOUNDARIES ------------------------------------------------

q_vec <- as_vector(res$quartile_preds$pelvic_pain$pelvic_pain_pv) %>% unique()
names(q_vec) <- names(res$quartile_boundaries$pelvic_pain[-1])

pdata_q <-
  res$quartile_preds$pelvic_pain %>%
  mutate(
    tile_group = cut(
      pelvic_pain_pv,
      breaks = c(-Inf, q_vec),
      labels = c(names(q_vec)),
      include.lowest = TRUE
    ),
    tile_group_label = paste0(tile_group, " (", round(pelvic_pain_pv, 2), ")")
  ) %>%
  filter(tile_group != "50%")

label_lookup_q  <- setNames(pdata_q$tile_group_label, pdata_q$tile_group)
colors_q        <- pal_jco("default")(3)[c(1, 3, 2)]
names(colors_q) <- unique(pdata_q$tile_group)

pred_q_plot <-
  ggplot(
    pdata_q,
    aes(yrs_since_baseline, pred, group = tile_group, color = tile_group, fill = tile_group)
  ) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 1/2, linewidth = .5) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 1/3, color = NA) +
  geom_line(linewidth = CONFIG$lw) +
  labs(
    x     = "Years Since Baseline Visit",
    y     = "Predicted MMH (PC1 Factor Score)",
    color = "Pelvic Pain\nQuartile Upper Boundary",
    fill  = "Pelvic Pain\nQuartile Upper Boundary"
  ) +
  scale_color_manual(values = colors_q, labels = label_lookup_q) +
  scale_fill_manual(values = colors_q, labels = label_lookup_q) +
  y_scale + y_coord +
  theme_classic() +
  theme(
    axis.text.x  = element_text_minor,
    axis.title.x = element_text_major,
    axis.text.y  = element_text_minor,
    axis.title.y = element_text_major,
    legend.position        = "bottom",
    legend.background      = element_rect(color = "black", linewidth = CONFIG$legend_lw),
    legend.box             = "vertical",
    legend.box.just        = "center",
    legend.title.position  = "top",
    legend.text = element_text_minor,
    legend.title = element_text(
      hjust = .5, size = CONFIG$minor_font_size, family = CONFIG$font_family,
      color = CONFIG$font_color, face = "bold"
    ),
    legend.key.size = unit(CONFIG$lks, "cm")
  )

# assembles
fig2 <-
  wrap_plots(list(obs_q_plot, pred_q_plot), ncol = 2) +
  plot_annotation(tag_levels = c("A", "B"), tag_suffix = ")") &
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
f <- file.path("output", "manuscript", "lmm-quartile-plot")
save_figure(f = f, p = fig2, w = 6.5, h = 4, units = "in", dpi = 300, both = TRUE)

# TABLE OF FIXED EFFECTS =======================================================

coef_mat <- coef(summary(fit2))
p_table <-
  as.data.frame(coef_mat) %>%
  rownames_to_column("term") %>%
  as_tibble() %>%
  rename(estimate = Estimate, se = `Std. Error`, df = df, t = `t value`, p = `Pr(>|t|)`) %>%
  mutate(lwr = estimate - 1.96 * se, upr = estimate + 1.96 * se) %>%
  mutate(
    term = case_when(
      term == "(Intercept)"                                     ~ "Intercept",
      term == "tanner_breast_gmc"                               ~ "Tanner Stage - Breast (Between-Subject Effect)",
      term == "tanner_breast_wc"                                ~ "Tanner Stage - Breast (Within-Subject Change)",
      term == "tanner_hair_gmc"                                 ~ "Tanner Stage - Hair (Between-Subject Effect)",
      term == "tanner_hair_wc"                                  ~ "Tanner Stage - Hair (Within-Subject Change)",
      term == "yrs_since_baseline"                              ~ "Time",
      term == "menst_pain_pv"                                   ~ "Menstrual Pain",
      term == "pelvic_pain_pv"                                  ~ "Pelvic Pain",
      term == "yrs_since_baseline:menst_pain_pv"                ~ "Time * Menstrual Pain",
      term == "yrs_since_baseline:pelvic_pain_pv"               ~ "Time * Pelvic Pain",
      term == "menst_pain_pv:pelvic_pain_pv"                    ~ "Menstrual * Pelvic Pain",
      term == "yrs_since_baseline:menst_pain_pv:pelvic_pain_pv" ~ "Time * Menstrual * Pelvic Pain",
      .default = NA_character_
    )
  ) %>%
  select(Term = term, b = estimate, LL = lwr, UL = upr, SE = se, df, t, p)

ft_p_table <-
  flextable(p_table) %>%
  add_header_row(
    values = c("", "", "95% CI", "", "", "", ""),
    colwidths = c(1, 1, 2, 1, 1, 1, 1)
  ) %>%
  bg(i = seq(2, nrow(p_table), by = 2), bg = "#F0F0F0", part = "body") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  bold(i = ~ p < 0.05, part = "body") %>%
  align(align = "left", part = "all") %>%
  hline(i = 1, j = 1:8, border = fp_border(width = 0), part = "header") %>%
  hline(i = 1, j = c(3, 4), border = fp_border(width = 1), part = "header") %>%
  colformat_double(j = c("b", "LL", "UL", "SE", "df", "t"), digits = 3) %>%
  colformat_double(j = "p", digits = 3) %>%
  add_footer_lines(
    "Note. CI=confidence interval; LL=lower level; UL=upper level. Between-subject effects for Tanner stage were grand mean centered across participants; within-subject change reflects deviation from each participant's mean Tanner stage (0 = no deviation)."
  ) %>%
  autofit()

message("=============================")
message("=== SAVING OUT LMM TABLE  ===")
message("=============================")
message("")
f <- file.path("output", "manuscript", "lmm-p-terms-table.csv")
write_csv(x = p_table, file = f)

f <- file.path("output", "manuscript", "lmm-p-terms-table.docx")
save_as_docx(ft_p_table, path = f)

# model fit summary (AIC, random effects variance)
cat("\n--- Model comparison (fit1 vs fit2) ---\n")
print(res$model_comparison)
cat("\n--- fit2 summary ---\n")
print(summary(fit2))

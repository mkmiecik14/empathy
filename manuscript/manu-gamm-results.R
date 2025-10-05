# manu-gamm-results.R
# Matt Kmiecik
# Purpose: generate figures / tables for the manuscript wrt GAMM results

# libraries ----
library(tidyverse); library(patchwork); library(RColorBrewer); library(ghibli)
library(grid)

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
  lw = .25 # line width
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
min(mod_data$PC1)
max(mod_data$PC1)

# line plot of supplementary proj. across time (i.e., df$plots$p3)
obs_mmh_plot <- 
  ggplot(mod_data, aes(yrs_since_baseline, PC1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(aes(group = subject_id), alpha = 1/3) +
  geom_smooth(
    method = "lm", se = TRUE, color = ghibli_palettes$MononokeMedium[3], alpha = 1/2, 
    fill = ghibli_palettes$MononokeMedium[3] 
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
obs_mmh_plot 

# predicted MMH over time
pdata <- df$preds$time %>% as_tibble()
pred_mmh_time <- 
  ggplot(pdata, aes(yrs_since_baseline, pred_resp)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(
    aes(ymin = lwr_resp, ymax = upr_resp), 
    alpha = 1/3, fill = ghibli_palettes$MononokeMedium[5]
    ) +
  geom_line(color = ghibli_palettes$MononokeMedium[5]) +
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
pred_mmh_time

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
  geom_line(color = ghibli_palettes$MononokeMedium[4]) +
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
pred_mmh_pp 

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
fig

# save out here
f <- file.path("output", "manuscript", "long-mmh-plot")
save_figure(f = f, p = fig, w = 6.5, h = 3, units = "in", dpi = 300, both = TRUE)


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
f <- file.path("output", "manuscript", "gamm-p-terms-table.csv")
write_csv(x = p_table, file = f)

# write out a cat() of R2 and AIC values for both models:

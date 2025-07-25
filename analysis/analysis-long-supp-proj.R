# analysis-long-supp-proj.R
# Matt Kmiecik
# Started 2025-07-22
# Current version: v1
# Purpose: compute MMH longitudinally via supplementary projections and examine
# change over time

# grabs versioning info ----
source("src/fns/versioning_proc.R")
vinfo <- 
  versioning_proc(testing = TRUE, this_script = "analysis-long-supp-proj")

# libraries ----
library(tidyverse); library(TInPosition); library(ggrepel)

# data ----

## PCA data
f <- file.path("output", "pca-data.rds")
dd <- read_rds(file = f)

# these cols need to be flipped so that increased scores == more sensitive
tcols <- c("knee_PPT", "shoulder_PPT", "knee_CPM", "shoulder_CPM")
dd_flip <- 
  dd %>% 
  mutate(across(.cols = all_of(tcols), .fns = ~-1*.x)) %>% # flips
  select(-shoulder_CPM) # shoulder CPM was dropped in PCA analysis 3
  
## Previous PCA models

# experimental only
f <- file.path("output", "analysis-pca-3-exp-res.rds")
exp_pca_res <- read_rds(file = f)

# experimental + questionnaires
f <- file.path("output", "analysis-pca-3-q-res.rds")
q_pca_res <- read_rds(file = f)


# analysis ----

# creates data sets
dd_visit12 <- dd_flip %>% filter(!grepl("baseline", redcap_event_name)) # not baseline
dd_visit12_c <- dd_visit12 %>% filter(complete.cases(.)) # complete data
dd_visit12_nc <- dd_visit12 %>% filter(!complete.cases(.)) # missing data

# formatting complete data for supplementary projection
supp_data <- 
  dd_visit12_c %>% 
  mutate(
    redcap_event_name = case_when(
      grepl("visit_1", redcap_event_name) ~ "v1",
      grepl("visit2", redcap_event_name) ~ "v2",
      .default = NA
      )
    ) %>%
  mutate(id = paste0(subject_id, "_", redcap_event_name), .before = subject_id)
supp_data <- as.data.frame(supp_data) # creates data frame
rownames(supp_data) <- supp_data$id # sets rownames to id + visit
supp_data <- as.matrix(supp_data[, -c(1:3)]) # creates matrix

# creating supplementary proj data for experimental only
q_cols <- c("cssi", "bodymap", "gss", "hsc_mean") # questionnaire columns
supp_data_e <- supp_data[, base::setdiff(colnames(supp_data), q_cols)]

# computing supplementary projections for each participant at each visit

# supplementary projections of experimental only
supp_res_exp <- 
  supplementaryRows(
    SUP.DATA = supp_data_e, 
    res = exp_pca_res$pca_res$Fixed.Data
    )

# supplementary projections of questionnaires
supp_res_q <- 
  supplementaryRows(SUP.DATA = supp_data, res = q_pca_res$Fixed.Data)

# combining observed FI with supplemental projections

# function that helps extract fi data and combine with supp projs
extract_fi <- function(pca_fi_data, supp_fi_data){
  fi <- pca_fi_data                 # isolates fi data
  ss <- paste0(rownames(fi), "_v0") # appends visit info to ss
  rownames(fi) <- ss                # updates rownames
  fi <- rbind(fi, supp_fi_data)     # rowbinds with supp projections
  # creates tibble
  fi_tib <- 
    as_tibble(fi, rownames = "id") %>% 
    separate(id, into = c("subject_id", "visit")) %>%
    mutate(visit = as.numeric(gsub("v", "", visit)))
  colnames(fi_tib) <- gsub("V", "PC", colnames(fi_tib))
  return(fi_tib)
}

# extracts fi data and combines with supplementary projections
exp_fi_tib <- 
  extract_fi(exp_pca_res$pca_res$Fixed.Data$ExPosition.Data$fi, supp_res_exp$fii)
q_fi_tib <- 
  extract_fi(q_pca_res$Fixed.Data$ExPosition.Data$fi, supp_res_q$fii)

# function that helps plot the fis over time/visit
plot_fi_time <- function(data, pcs = c(1,2), axs_rng = c(-12, 12)){
  # preps plotting data
  pdata <- 
    data %>% 
    select(subject_id, visit, all_of(c(paste0("PC", pcs)))) %>%
    mutate(visit = factor(visit, levels = c(0, 1, 2)))
  
  # sets colors for each visit
  tcols <- c("0" = "white", "1" = "grey", "2" = "black")
  
  # plots
  p <- 
    ggplot(
      pdata, 
      aes(
        !!sym(paste0("PC", pcs[1])), !!sym(paste0("PC", pcs[2])), 
        group = subject_id, fill = visit
        )
      ) +
    geom_vline(xintercept = 0, linetype = 3) +
    geom_hline(yintercept = 0, linetype = 3) +
    geom_path(alpha = 1/2) +
    geom_point(shape = 21, color = "black") +
    theme_minimal() +
    #geom_text_repel(aes(label = subject_id), show.legend = FALSE) +
    coord_cartesian(xlim = axs_rng, ylim = axs_rng) +
    scale_fill_manual(values = tcols) + 
    labs(x = paste0("PC", pcs[1]), y = paste0("PC", pcs[2])) +
    theme(
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  return(p)
  
}

# determining axis range of first 3 components
exp_fi <- exp_pca_res$pca_res$Fixed.Data$ExPosition.Data$fi
q_fi <- q_pca_res$Fixed.Data$ExPosition.Data$fi
min(apply(exp_fi[,c(1:3)], 2, min))
max(apply(exp_fi[,c(1:3)], 2, max))
min(apply(q_fi[,c(1:3)], 2, min))
max(apply(q_fi[,c(1:3)], 2, max))

plot_fi_time(data = exp_fi_tib, pcs = c(1, 2), axs_rng = c(-8, 8))  
plot_fi_time(data = exp_fi_tib, pcs = c(2, 3), axs_rng = c(-8, 8)) 
plot_fi_time(data = exp_fi_tib, pcs = c(1, 3), axs_rng = c(-8, 8)) 

plot_fi_time(data = q_fi_tib, pcs = c(1, 2), axs_rng = c(-8, 8))  
plot_fi_time(data = q_fi_tib, pcs = c(2, 3), axs_rng = c(-8, 8)) 
plot_fi_time(data = q_fi_tib, pcs = c(1, 3), axs_rng = c(-8, 8)) 

# plotting with time on the x-axis ----

# first step is to retrive the timestamps from redcap data
f <- file.path("data", "EH17338EMPATHY_DATA_2024-11-07_1140_noPHI.csv")
rc_data_raw <- read_csv(file = f)
rc_data <- 
  rc_data_raw %>% 
  select(subject_id, redcap_event_name, todaysdate) %>% 
  filter(!is.na(subject_id)) %>%
  mutate(
    todaysdate = mdy(todaysdate), subject_id = as.character(subject_id)
    ) %>%
  mutate(baseline_date = min(todaysdate), .by = subject_id) %>%
  mutate(
    yrs_since_baseline = time_length(interval(baseline_date, todaysdate), "years"),
    visit = case_when(
      grepl("baseline", redcap_event_name) ~ 0,
      grepl("visit_1", redcap_event_name) ~ 1,
      grepl("visit2", redcap_event_name) ~ 2,
      .default = NA
      )
    )
# data set to use in join
rc_data_time <- 
  rc_data %>% 
  select(subject_id, visit, todaysdate, baseline_date, yrs_since_baseline)

# joins fis with time data
exp_fi_time <- 
  left_join(exp_fi_tib, rc_data_time, by = c("subject_id", "visit")) %>%
  mutate(pca = "exp") # for join
q_fi_time <- 
  left_join(q_fi_tib, rc_data_time, by = c("subject_id", "visit")) %>%
  mutate(pca = "q")

# combines data
fi_wide <- 
  bind_rows(exp_fi_time, q_fi_time) %>% 
  relocate(pca, .before = subject_id) %>%
  relocate(todaysdate, baseline_date, yrs_since_baseline, .before = PC1)

# converts to long
fi_long <- 
  fi_wide %>% 
  pivot_longer(
    cols = -c(pca, subject_id, visit, todaysdate, baseline_date, yrs_since_baseline)
    )

# plot using time
rdgy <- RColorBrewer::brewer.pal(11, "RdGy")
pdata <- fi_long %>% filter(name %in% c(paste0("PC", 1:3))) %>% mutate(visit = factor(visit, levels = c(0:2))) 
ggplot(pdata, aes(yrs_since_baseline, value, group = subject_id)) +
  geom_line(alpha = 1/2) +
  geom_smooth(
    method = "lm", se = TRUE, aes(group = 1), color = rdgy[3], fill = rdgy[3]
    ) +
  labs(
    x = "Years since Baseline Visit", y = "Factor Scores (fi)",
    caption = "Note that positive factor scores for the Exp. PCA == more MMH\nOpposite is true for Q. PCA. "
    ) +
  theme_bw() +
  facet_grid(pca~name, labeller = label_both)
# it was found that ss 214 does not have a baseline

# plot using visit
# sets colors for each visit
tcols <- c("0" = "white", "1" = "grey", "2" = "black")
ggplot(pdata, aes(visit, value, fill = visit)) +
  geom_boxplot() +
  labs(
    x = "Years since Baseline Visit", y = "Factor Scores (fi)",
    caption = "Note that positive factor scores for the Exp. PCA == more MMH\nOpposite is true for Q. PCA. "
  ) +
  theme_bw() +
  scale_fill_manual(values = tcols) +
  facet_grid(pca~name, labeller = label_both)

ggplot(pdata %>% filter(pca == "exp"), aes(value)) +
  geom_histogram(binwidth = .2) +
  labs(
    x = "Years since Baseline Visit", y = "Frequency",
    caption = "Note that positive factor scores for the Exp. PCA == more MMH\nOpposite is true for Q. PCA. "
  ) +
  theme_bw() +
  facet_grid(pca~visit, labeller = label_both)

ggplot(pdata %>% filter(pca == "q"), aes(value)) +
  geom_histogram(binwidth = .2) +
  labs(
    x = "Years since Baseline Visit", y = "Frequency",
    caption = "Note that positive factor scores for the Exp. PCA == more MMH\nOpposite is true for Q. PCA. "
  ) +
  theme_bw() +
  facet_grid(pca~visit, labeller = label_both)

# Linear mixed modeling ----
library(mgcv)



# saving ----
# versioned_write_rds(data = [DATA GOES HERE], vi = vinfo) # writes out
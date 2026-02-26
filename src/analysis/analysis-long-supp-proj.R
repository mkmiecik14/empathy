# analysis-long-supp-proj.R
# Matt Kmiecik
# Started 2025-07-22
# Current version: v1
# Purpose: compute MMH longitudinally via supplementary projections and examine
# change over time

# grabs versioning info ----
source("src/fns/versioning_proc.R")
vinfo <- 
  versioning_proc(testing = FALSE, this_script = "analysis-long-supp-proj")

# libraries ----
library(tidyverse); library(TInPosition); library(ggrepel); library(patchwork)

# custom functions ----
source("src/fns/broom_gam.R")
source("src/fns/predict_link.R")

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

# stores plots into list
traj_plots <- 
  list(
    exp_12 = 
      plot_fi_time(data = exp_fi_tib, pcs = c(1, 2), axs_rng = c(-8, 8)) + 
      ggtitle("Experimental PCA"),
    exp_23 = 
      plot_fi_time(data = exp_fi_tib, pcs = c(2, 3), axs_rng = c(-8, 8)) +
      ggtitle("Experimental PCA"),
    exp_13 = plot_fi_time(data = exp_fi_tib, pcs = c(1, 3), axs_rng = c(-8, 8)) +
      ggtitle("Experimental PCA"),
    q_12 = plot_fi_time(data = q_fi_tib, pcs = c(1, 2), axs_rng = c(-8, 8)) +
      ggtitle("Questionnaire PCA"),
    q_23 = plot_fi_time(data = q_fi_tib, pcs = c(2, 3), axs_rng = c(-8, 8)) +
      ggtitle("Questionnaire PCA"),
    q_13 = plot_fi_time(data = q_fi_tib, pcs = c(1, 3), axs_rng = c(-8, 8)) +
      ggtitle("Questionnaire PCA")
)

# plotting with time on the x-axis ----

# first step is to retrieve the timestamps from redcap data
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
lplot <- 
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
bplot <- 
  ggplot(pdata, aes(visit, value, fill = visit)) +
  geom_boxplot() +
  labs(
    x = "Years since Baseline Visit", y = "Factor Scores (fi)",
    caption = "Note that positive factor scores for the Exp. PCA == more MMH\nOpposite is true for Q. PCA. "
  ) +
  theme_bw() +
  scale_fill_manual(values = tcols) +
  facet_grid(pca~name, labeller = label_both)

hplot_exp <- 
  ggplot(pdata %>% filter(pca == "exp"), aes(value)) +
  geom_histogram(binwidth = .5) +
  labs(
    x = "Years since Baseline Visit", y = "Frequency",
    caption = "Note that positive factor scores for the Exp. PCA == more MMH\nOpposite is true for Q. PCA. ",
    title = "Experimental PCA"
  ) +
  theme_bw() +
  facet_grid(name~visit, labeller = label_both)

hplot_q <- 
  ggplot(pdata %>% filter(pca == "q"), aes(value)) +
  geom_histogram(binwidth = .5) +
  labs(
    x = "Years since Baseline Visit", y = "Frequency",
    caption = "Note that positive factor scores for the Exp. PCA == more MMH\nOpposite is true for Q. PCA. ",
    title = "Questionnaire PCA"
  ) +
  theme_bw() +
  facet_grid(name~visit, labeller = label_both)

# storing plots into list
traj_plots_2 <- list(
  line_plot = lplot, box_plot = bplot, hist_exp = hplot_exp, hist_q = hplot_q
)

# Linear mixed modeling ----
# Examining change over time in pain variables as covariates
library(mgcv); library(itsadug)
fi_long$subject_id <- factor(fi_long$subject_id) # converts ss to factor

## brings in pain data and processes further
pain_data <- 
  read_rds("output/prepro-long-pain-v1.rds") %>%
  mutate(
    visit = case_when(
      grepl("visit_1", redcap_event_name) ~ 1, 
      grepl("visit2", redcap_event_name) ~ 2, 
      .default = NA
      ),
    subject_id = factor(subject_id)
    ) %>%
  # computes pelvic pain both original and transformed
  rowwise() %>%
  mutate(pelvic_pain = mean(c_across(matches("q2|q3|q4")))) %>%
  ungroup() %>%
  # work to calculate composite using inverse hyperbolic sine transformation
  # using asinh()
  mutate(
    across(.cols = matches("q2|q3|q4"), .fns = ~asinh(.x), .names = "{.col}_trans") 
  ) %>%
  rowwise() %>%
  mutate(pelvic_pain_trans = mean(c_across(ends_with("trans"))))

# selects data for join
pain_data_join <- 
  pain_data %>% 
  select(
    subject_id, visit, menst_pain = painrating_child_q1, pelvic_pain, 
    pelvic_pain_trans
    )

# joins longitudinal PCA data with menstrual and pelvic pain data
# fi_long_pain <- 
#   fi_long %>% left_join(., pain_data_join, by = c("subject_id", "visit"))

# the above effectively discards all baseline data in gamms, so we will
# first attempt with PV1 menstrual and pelvic pain data
fi_long_pain <- 
  fi_long %>% 
  left_join(
    ., 
    pain_data_join %>% filter(visit == 1) %>% select(-visit), 
    by = c("subject_id")
    )

# attempting to run the models efficiently using lists
data_list <- 
  fi_long_pain %>%
  filter(name %in% c(paste0("PC", 1:3))) %>%
  split(interaction(.$pca, .$name, sep = "_"))
gamm1 <- gamm2 <- gamm3 <- gamm4 <- list()
for (i in seq_along(data_list)) {
  gamm1[[i]] <-
    gam(
      value ~ 1 + yrs_since_baseline + menst_pain + pelvic_pain + 
        s(subject_id, bs = "re"), 
      data = data_list[[i]], 
      method = "REML"
      )
  gamm2[[i]] <-
    gam(
      value ~ 1 + yrs_since_baseline * (menst_pain + pelvic_pain) +
        s(subject_id, bs = "re"), 
      data = data_list[[i]], 
      method = "REML"
    )
  gamm3[[i]] <- 
    gam(
      value ~ 1 + s(yrs_since_baseline) + s(menst_pain) + s(pelvic_pain) + 
        s(subject_id, bs = "re"), 
      data = data_list[[i]], 
      method = "REML"
    )
  gamm4[[i]] <-
    gam(
      value ~ 1 + s(yrs_since_baseline) + s(menst_pain) + s(pelvic_pain) + 
        ti(yrs_since_baseline, menst_pain) + 
        ti(yrs_since_baseline, pelvic_pain) + s(subject_id, bs = "re"), 
    data = data_list[[i]], 
    method = "REML"
  )
  print(paste0("Finished modeling ", names(data_list)[[i]]))
}
# names each entry of the list by PCA and PC
names(gamm4) <- names(gamm3) <- names(gamm2) <- names(gamm1) <- names(data_list)

# procedure for extracting omnibus and estimates (both parametric and smooth)
gamms <- list(gamm1, gamm2, gamm3, gamm4) # combies gamms into list
gamm_ests_s <- gamm_ests_para <- gamm_omni <- list() # preallocates list
for (i in seq_along(gamms)) {
  # omnibus
  gamm_omni[[i]] <- 
    gamms[[i]] %>% 
    map(~glance_gam(.x)) %>% 
    list_rbind(names_to = "meas") %>% 
    mutate(gamm = paste0("gamm", i))
  # estimates (parametric)
  gamm_ests_para[[i]] <- 
    gamms[[i]] %>% 
    map(~tidy_gam(.x)) %>% 
    list_rbind(names_to = "meas") %>% 
    mutate(gamm = paste0("gamm", i))
  # smooth terms
  gamm_ests_s[[i]] <- 
    gamms[[i]] %>% 
    map(~tidy_gam(.x, type = "smooth")) %>% 
    list_rbind(names_to = "meas") %>% 
    mutate(gamm = paste0("gamm", i))
}

# combines into dataframes
omni <- list_rbind(gamm_omni) %>% separate(meas, into = c("pca", "pc"))
ests_p <- list_rbind(gamm_ests_para) %>% separate(meas, into = c("pca", "pc"))
ests_s <- list_rbind(gamm_ests_s) %>% separate(meas, into = c("pca", "pc"))

## plotting AIC
aic_plot <- 
  ggplot(omni, aes(gamm, AIC)) +
  geom_point() +
  geom_line(aes(group = 1)) +
  labs(x = "GAMM") +
  theme_bw() +
  facet_grid(pca~pc, labeller = label_both)

## plotting R^2
r2_plot <- 
  ggplot(omni, aes(gamm, r.sq)) +
  geom_point() +
  geom_line(aes(group = 1)) +
  theme_bw() +
  labs(x = "GAMM", y = expression(R^2)) +
  facet_grid(pca~pc, labeller = label_both)

# storing plots in a list
mod_comp_plots <- list(aic = aic_plot, r2 = r2_plot)

# function that helps generate new data; allowing some variables to vary
# from min - max, others will stay the mean, others can be customized
make_newdata <- function(model, vars_to_vary, length_out = 200, custom_fixed = list()) {
  # Extract the data used in the model
  model_data <- model.frame(model)
  all_vars <- names(model_data)
  
  # Check: are variables to vary in the model?
  if (!all(vars_to_vary %in% all_vars)) {
    stop("Some variables to vary are not in the model data.")
  }
  
  # Build the list of values to vary
  vary_list <- lapply(vars_to_vary, function(var) {
    var_data <- model_data[[var]]
    if (is.numeric(var_data)) {
      seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), length.out = length_out)
    } else if (is.factor(var_data)) {
      levels(var_data)
    } else {
      stop(paste("Variable", var, "must be numeric or factor."))
    }
  })
  names(vary_list) <- vars_to_vary
  
  # Identify other variables to hold constant
  fixed_vars <- setdiff(all_vars, vars_to_vary)
  
  # Build the fixed list using means or reference levels unless overridden
  fixed_list <- lapply(fixed_vars, function(var) {
    if (var %in% names(custom_fixed)) {
      # Use user-provided custom value
      return(custom_fixed[[var]])
    }
    
    var_data <- model_data[[var]]
    if (is.numeric(var_data)) {
      mean(var_data, na.rm = TRUE)
    } else if (is.factor(var_data)) {
      levels(var_data)[1]
    } else {
      stop(paste("Variable", var, "must be numeric or factor."))
    }
  })
  names(fixed_list) <- fixed_vars
  
  # Combine into a full newdata list
  newdata_list <- c(vary_list, fixed_list)
  
  # Return data frame for prediction
  res <- expand.grid(newdata_list)
  return(res)
}

# Named list of GAMM lists
gamm_lists <- list(gamm1 = gamm1, gamm2 = gamm2, gamm3 = gamm3, gamm4 = gamm4)

# Variables to vary
vary_vars <- c("yrs_since_baseline", "menst_pain", "pelvic_pain")

# Combine everything using cross-product
# Expand all combinations of model group and variable
model_var_combos <- 
  expand_grid(
    group = names(gamm_lists),
    var = vary_vars
    )

# For each (group, var), generate predictions across all models inside the group
all_preds <- 
  pmap_dfr(
    model_var_combos,
    function(group, var) {
      model_list <- gamm_lists[[group]]
      
      # Create newdata for each model in the list
      new_data_list <- 
        map(
          model_list,
          ~make_newdata(
            .x, vars_to_vary = var, custom_fixed = list(subject_id = "ss")
            )
          )
      
      # Predict for each model using the new data
      map2_dfr(
        model_list, new_data_list,
        ~predict_link(.x, .y) %>% as_tibble(),
        .id = "pca_pc"
        ) %>%
        mutate(group = group, var = var)
      }
    ) %>%
  separate(pca_pc, into = c("pca", "pc"))

# converts to factor to reorder resultant plots
all_preds$var <- 
  factor(
    all_preds$var, levels = c("yrs_since_baseline", "pelvic_pain", "menst_pain")
    )

plot_predictions <- function(
    preds_df,
    pc = "PC1",
    pca = "exp",
    group = NULL,
    var_to_plot = NULL,
    facet_by = NULL,
    scales = "free_x"
) {
  # Filter by PC, pca, group
  df <- preds_df %>%
    filter(pc == !!pc, pca == !!pca) %>%
    { if (!is.null(group)) filter(., group == !!group) else . }
  
  # If var_to_plot not provided, infer
  if (is.null(var_to_plot)) {
    vars <- unique(df$var)
    if (length(vars) > 1 && is.null(facet_by)) {
      stop("Multiple variables found. Please provide `var_to_plot` or set `facet_by`.")
    }
    var_to_plot <- vars[1]
  }
  
  # Filter to just that variable
  df <- df %>% filter(var == var_to_plot)
  
  # Extract x-values
  df <- df %>%
    mutate(x = .[[var_to_plot]])
  
  # Plot
  p <- ggplot(df, aes(x = x, y = pred_resp)) +
    geom_ribbon(aes(ymin = lwr_resp, ymax = upr_resp), alpha = 0.3) +
    geom_line() +
    labs(
      x = var_to_plot,
      y = "Predicted Response (Factor Score)",
      caption = "95% CI error shading"
    ) +
    theme_bw()
  
  # Optional faceting
  if (!is.null(facet_by)) {
    facet_formula <- as.formula(paste("~", paste(facet_by, collapse = "+")))
    p <- p + facet_wrap(facet_formula, scales = scales, nrow = 1)
  }
  
  return(p)
}

combos <- 
  expand.grid(
    pca = c("exp", "q"),
    pc = paste0("PC",1:3),
    var = c("yrs_since_baseline", "pelvic_pain", "menst_pain")
    )

# generates plots
pred_plots <- 
  pmap(
    combos, 
    function(pca, pc, var) 
      plot_predictions(
        all_preds, pca = pca, pc = pc, var = var, facet_by = "group"
        )
    )
# names them for easy access
names(pred_plots) <- paste(combos$pca, combos$pc, combos$var, sep = "_")

plot_names <- names(pred_plots)
plot_info <- data.frame(
  name = plot_names,
  pca = sapply(strsplit(plot_names, "_"), `[`, 1),
  pc = sapply(strsplit(plot_names, "_"), `[`, 2),
  var = sapply(strsplit(plot_names, "_"), `[`, 3),
  stringsAsFactors = FALSE
)


# Helper function for organizing prediction plots
organize_pred_plots <- function(plot_list) {
  # Parse plot names to extract pca, pc, and variable
  plot_names <- names(plot_list)
  plot_info <- data.frame(
    name = plot_names,
    pca = sapply(strsplit(plot_names, "_"), `[`, 1),
    pc = sapply(strsplit(plot_names, "_"), `[`, 2),
    var = sapply(strsplit(plot_names, "_"), `[`, 3),
    stringsAsFactors = FALSE
  )
  
  # Create composite plots: each PCA-PC combination gets 3 variables stacked vertically
  pca_pc_combos <- unique(plot_info[, c("pca", "pc")])
  
  composite_plots <- list()
  for (i in seq_len(nrow(pca_pc_combos))) {
    pca_val <- pca_pc_combos$pca[i]
    pc_val <- pca_pc_combos$pc[i]
    
    # Get plots for this PCA-PC combination
    matching_plots <- 
      plot_info$name[plot_info$pca == pca_val & plot_info$pc == pc_val]
    
    # Order by variable: yrs_since_baseline, pelvic_pain, menst_pain
    var_order <- c("yrs_since_baseline", "pelvic_pain", "menst_pain")
    ordered_plots <- 
      matching_plots[
        order(
          match(plot_info$var[match(matching_plots, plot_info$name)], var_order)
          )
        ]
    
    # Create composite plot with variables stacked vertically
    composite_name <- paste(pca_val, pc_val, sep = "_")
    composite_plots[[composite_name]] <- wrap_plots(
      plot_list[ordered_plots], 
      ncol = 1, 
      nrow = 3
    ) + plot_annotation(
      title = paste(toupper(pca_val), "PCA -", pc_val),
      theme = theme(plot.title = element_text(hjust = 0.5))
    )
  }
  
  # Also group by variable type (original functionality)
  by_var <- split(plot_list, sapply(strsplit(names(plot_list), "_"), `[`, 3))
  by_var_composite <- map(by_var, ~wrap_plots(.x, nrow = 2))
  
  return(list(
    individual = plot_list, 
    composite_by_pca_pc = composite_plots,
    composite_by_variable = by_var_composite
  ))
}

# organizes the pred plots
pred_plots_org  <- organize_pred_plots(pred_plots)

# Create comprehensive plots list
analysis_plots <- list(
  trajectory_plots = traj_plots,
  trajectory_plots_2 = traj_plots_2,
  model_comparison = mod_comp_plots,
  prediction_plots = pred_plots_org
  )

# Compile all results
results <- 
  list(
    data = fi_long_pain,
    model_results = 
      list(
        omni = omni, 
        ests_parametric = ests_p, 
        ests_smooth = ests_s
        ),
    predictions = all_preds,
    plots = analysis_plots,
    models = gamm_lists
  )

# saving ----
versioned_write_rds(data = results, vi = vinfo)
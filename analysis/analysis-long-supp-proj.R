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
  geom_histogram(binwidth = .5) +
  labs(
    x = "Years since Baseline Visit", y = "Frequency",
    caption = "Note that positive factor scores for the Exp. PCA == more MMH\nOpposite is true for Q. PCA. "
  ) +
  theme_bw() +
  facet_grid(name~visit, labeller = label_both)

ggplot(pdata %>% filter(pca == "q"), aes(value)) +
  geom_histogram(binwidth = .5) +
  labs(
    x = "Years since Baseline Visit", y = "Frequency",
    caption = "Note that positive factor scores for the Exp. PCA == more MMH\nOpposite is true for Q. PCA. "
  ) +
  theme_bw() +
  facet_grid(name~visit, labeller = label_both)

# Linear mixed modeling ----
library(mgcv); library(itsadug)
fi_long$subject_id <- factor(fi_long$subject_id) # converts ss to factor

# computes generalized additive models with random effect of intercept
mod <- 
  fi_long %>% 
  filter(name %in% c(paste0("PC", 1:3))) %>% # only first three PCs
  nest_by(pca, name) %>%
  mutate(
    mod = list( # LINEAR EFFECT OF TIME
      gam(
        value ~ 1 + yrs_since_baseline + s(subject_id, bs = "re"), 
        data = data, 
        method = "REML"
        )
      ),
      mod2 = list( # NON-LINEAR EFFECT OF TIME
        gam(
          value ~ 1 + s(yrs_since_baseline) + s(subject_id, bs = "re"),
          data = data, 
          method = "REML"
        )
        )
    )

# retrieves omnibus model stats and estimates from gams
mod_omni <- mod %>% reframe(glance_gam(mod))
mod_ests <- mod %>% reframe(tidy_gam(mod))
mod2_omni <- mod %>% reframe(glance_gam(mod2))
mod2_ests <- mod %>% reframe(tidy_gam(mod2))

# combines into one df
omni <- bind_rows(mod_omni, mod2_omni, .id = "model")
ests <- bind_rows(mod_ests, mod2_ests, .id = "model")

# retrieves REML tests and AIC comparison
aic_res <- vector("list", length = nrow(mod))
names(aic_res) <- paste(mod$pca, mod$name, sep = "_")
for (i in seq_along(aic_res)) {
  aic_res[[i]] <- compareML(mod$mod[[i]], mod$mod2[[i]])  
}

# combines info from all models
aic <- 
  aic_res %>% 
  map("table") %>%
  list_rbind(names_to = "mod") %>%
  mutate(model = if_else(Model == "mod$mod[[i]]", 1, 2)) %>%
  separate(mod, into = c("pca", "name"))

# plots AIC
ggplot(omni, aes(model, AIC)) +
  geom_bar(
    stat = "identity", color = "black", fill = "grey", position = "dodge", 
    width = .5
  ) +
  geom_text(
    data = aic %>% filter(model == 2), 
    aes(label = paste0("p=",p.value), y = 1525, x = 1.5)
    ) +
  coord_cartesian(ylim = c(1325, 1525)) +
  theme_bw() +  
  facet_grid(pca~name)

# getting "new data" for predictions
new_data <- 
  with(
    fi_long, 
    expand.grid(
      subject_id = "theoretical_id",
      yrs_since_baseline = seq(
        min(yrs_since_baseline, na.rm = TRUE),
        max(yrs_since_baseline, na.rm = TRUE),
        length.out = 200
        )
    )
    )

# generates predictions
pred <- mod %>% reframe(predict_link(mod, newdat = new_data))
pred_mod2 <- mod %>% reframe(predict_link(mod2, newdat = new_data))

# function to plot predictions
plot_preds <- function(dat){
  pmed <- ghibli::ghibli_palettes$PonyoMedium
  tcols <- c(PC1 = pmed[3], PC2 = pmed[5], PC3 = pmed[7])
  p <- 
    ggplot(dat, aes(yrs_since_baseline, pred_resp, color = name, fill = name)) +
    geom_point(
      data = fi_long %>% filter(name %in% paste0("PC", 1:3)), 
      aes(y = value), shape = 19, alpha = 1/3
      ) +
    geom_ribbon(aes(ymin = lwr_resp, ymax = upr_resp), alpha = 1/2) +
    geom_line() +
    scale_fill_manual(values = tcols) +
    scale_color_manual(values = tcols) +
    labs(
      x = "Years Since Baseline Visit", 
      y = "Predicted Response (Factor Score)", 
      caption = "95% CI shading\npoints are observed data."
    ) +
    theme_bw() +
    facet_grid(pca~name, labeller = label_both) +
    theme(legend.position = "none")
  return(p)
}

# plots predictions 
plot_preds(dat = pred) # linear effect of time
plot_preds(dat = pred_mod2) # smoothed effect of time

# Examining change over time in pain variables as covariates ----

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

## modeling
mod_data <- fi_long_pain %>% filter(pca == "exp", name == "PC1")
gamm1 <- 
  gam(
    value ~ 1 + yrs_since_baseline + menst_pain + pelvic_pain + s(subject_id, bs = "re"), 
    data = mod_data, 
    method = "REML"
    )
summary(gamm1)
plot.gam(gamm1)

gamm2 <- 
  gam(
    value ~ 1 + yrs_since_baseline * (menst_pain + pelvic_pain) + s(subject_id, bs = "re"), 
    data = mod_data, 
    method = "REML"
  )
summary(gamm2)
plot.gam(gamm2)
compareML(gamm1, gamm2)

modeled_data <- model.frame(gamm1) %>% as_tibble()
new_data <- 
  with(
    modeled_data, 
    expand.grid(
      subject_id = "theorectical_ss",
      yrs_since_baseline = seq(
        min(yrs_since_baseline), max(yrs_since_baseline), length.out = 200
        ),
      menst_pain = c(
        mean(menst_pain), 
        mean(menst_pain) + 2*sd(menst_pain), 
        mean(menst_pain) - 2*sd(menst_pain)
        ),
      pelvic_pain = c(
        mean(pelvic_pain), 
        mean(pelvic_pain) + 2*sd(pelvic_pain), 
        mean(pelvic_pain) - 2*sd(pelvic_pain)
      )
      )
    ) %>%
  as_tibble()

preds <- predict_link(mod = gamm1, newdat = new_data) %>% as_tibble()

mpain <- sort(unique(preds$menst_pain))
pp <- sort(unique(preds$pelvic_pain))

pdata <- 
  preds %>% 
  filter(menst_pain %in% mpain[2]) %>% 
  mutate(pelvic_pain = factor(pelvic_pain))
ggplot(
  pdata, # at average menstrual pain
  aes(yrs_since_baseline, pred_resp, group = pelvic_pain, color = pelvic_pain)
  ) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr_resp, ymax = upr_resp), alpha = 1/2) +
  theme_bw()

pdata <- fi_long_pain %>% filter(pca == "exp", name == "PC1")
ggplot(pdata, aes(pelvic_pain, value)) +
  geom_point(shape = 19, alpha = 1/2, position = "jitter") +
  geom_smooth(method = "lm", se = TRUE, color = rdgy[3]) +
  geom_rug(length = unit(0.01, "npc"), alpha = .5, position = "jitter") +
  labs(x = "Pelvic Pain", y = "PC1 (MMH)") +
  theme_bw()

ggplot(pdata, aes(menst_pain, value)) +
  geom_point(shape = 19, alpha = 1/2, position = "jitter") +
  geom_smooth(method = "lm", se = TRUE, color = rdgy[3]) +
  geom_rug(length = unit(0.01, "npc"), alpha = .5, position = "jitter") +
  labs(x = "Menstrual Pain", y = "PC1 (MMH)") +
  theme_bw()

## looking at smooth effects
## modeling
mod_data <- fi_long_pain %>% filter(pca == "exp", name == "PC1")
gamm1 <- 
  gam(
    value ~ 1 + s(yrs_since_baseline) + s(menst_pain) + s(pelvic_pain) + s(subject_id, bs = "re"), 
    data = mod_data, 
    method = "REML"
  )
summary(gamm1)
par(mfrow = c(2,2))
plot.gam(gamm1)

gamm2 <- 
  gam(
    value ~ 1 + s(yrs_since_baseline) + s(menst_pain) + s(pelvic_pain) + 
      ti(yrs_since_baseline, menst_pain) + 
      ti(yrs_since_baseline, pelvic_pain) + s(subject_id, bs = "re"), 
    data = mod_data, 
    method = "REML"
  )
summary(gamm2)
par(mfrow = c(3, 2))
plot.gam(gamm2)
vis.gam(gamm2, view = c("yrs_since_baseline", "pelvic_pain"), plot.type = "persp", 
        theta = 29, phi = 10, color = "terrain", too.far = .1)

compareML(gamm1, gamm2)

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
ggplot(omni, aes(gamm, AIC)) +
  geom_point() +
  geom_line(aes(group = 1)) +
  theme_bw() +
  facet_grid(pca~pc, labeller = label_both)

## plotting R^2
ggplot(omni, aes(gamm, r.sq)) +
  geom_point() +
  geom_line(aes(group = 1)) +
  theme_bw() +
  facet_grid(pca~pc, labeller = label_both)

# next step is to visualize the models in the way that makes most sense for each
# some have interesting main effects and smooth effects. No interactions with time

# this one is specifically looking at the interesting effect of menstrual pain
# on PC3 q; 
# next step, see if you can write a function that chooses which variables to vary
# and which ones to keep constant
modeled_data <- model.frame(gamm4[[6]]) %>% as_tibble()
new_data <- 
  with(
    modeled_data, 
    expand.grid(
      subject_id = "theoretical_ss",
      yrs_since_baseline = mean(yrs_since_baseline),
      menst_pain = seq(min(menst_pain), max(menst_pain), length.out = 200),
      pelvic_pain = mean(pelvic_pain)
    )
    ) %>%
  as_tibble()
preds <- predict_link(mod = gamm4[[6]], newdat = new_data) %>% as_tibble()
ggplot(preds, aes(menst_pain, pred_resp)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr_resp, ymax = upr_resp), alpha = .5)

library(lme4); library(lmerTest)
mermod1 <- 
  lmer(
    value ~ 1 + yrs_since_baseline + menst_pain + pelvic_pain + (1 | subject_id),
    data = mod_data, 
    REML = TRUE
  )
summary(mermod1)      

mermod2 <- 
  lmer(
    value ~ 1 + yrs_since_baseline * (menst_pain + pelvic_pain) + (1 | subject_id),
    data = mod_data, 
    REML = TRUE
  )
summary(mermod2) 
anova(mermod1, mermod2)

mermod3 <- 
  lmer(
    value ~ 1 + yrs_since_baseline + menst_pain + pelvic_pain + (1 | subject_id) +
      (0 + yrs_since_baseline | subject_id),
    data = mod_data, 
    REML = TRUE
  )
summary(mermod3)
anova(mermod1, mermod3)

# saving ----
# versioned_write_rds(data = [DATA GOES HERE], vi = vinfo) # writes out
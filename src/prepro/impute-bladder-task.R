# impute-bladder-task.R
# Matt Kmiecik
# 2025-05-18

# Purpose: given the amount of missingness in the bladder task data, this script
# imputes misisng data

# libraries ----
library(tidyverse); library(mice); library(miceadds); library(pan);
library(broom.mixed)

# data ----

## reads in data
f <- file.path("output", "bladder-task-long-data.rds")
d <- read_rds(file = f)

## preps data for imputation
dd <- 
  d %>% 
  select(
    subject_id, redcap_event_name, bt_pain, bt_urgency, shared, pain, urgency
    ) %>%
  filter(shared %in% c(0, 6, 7, 8)) %>%
  # converts to stages for linear modeling
  mutate(shared = if_else(shared > 0, shared-4, shared+1)) %>%
  # treats longitudinal subjects as independent for imputation
  mutate(subject_id_event = paste0(subject_id, ".", redcap_event_name)) %>%
  mutate(
    subject_id_event = as.factor(subject_id_event), 
    subject_id_event_int = as.integer(subject_id_event)
  )

# remove these participants*visit given only 1 observation from pain and urgency
ss_rem <- 
  dd %>% 
  select(subject_id_event_int, pain, urgency) %>% 
  pivot_longer(cols = -subject_id_event_int) %>%
  filter(is.na(value)) %>%
  count(subject_id_event_int, name) %>%
  filter(n > 2) %>%
  pull(subject_id_event_int) %>%
  unique(.)

# creates data for imputation
datai <- dd %>% filter(!subject_id_event_int %in% ss_rem)

# quick visualization
ggplot(datai, aes(bt_pain, pain)) + geom_boxplot() + theme_bw()
ggplot(datai, aes(bt_pain, urgency)) + geom_boxplot() + theme_bw()

# subjects with missing data
ss_i_pain <- unique(datai$subject_id_event_int[is.na(datai$pain)])
ss_i_urg <- unique(datai$subject_id_event_int[is.na(datai$urgency)])

# imputation ----
# function for imputing data
impute_data <- function(data = datai, meas = "pain", maxit = 1, m = 20){
  
  # Define the predictor matrix
  pred <- make.predictorMatrix(data)
  pred[,] <- 0                # Start clean
  pred[meas, "shared"] <- 1  # Use time to predict value
  pred[meas, "subject_id_event_int"] <- -2   # Cluster variable
  
  # Set method
  meth <- make.method(data)
  meth[meas] <- "2l.pmm" # Predictive Mean Matching (PMM)
  
  # Run multiple imputation
  imp <- 
    mice(
      data, 
      method = meth, 
      m = m, 
      predictorMatrix = pred, 
      seed = 1218, 
      maxit = maxit
      )
  
  # Use one completed dataset
  completed_data <- as_tibble(complete(imp, 1))
  
  # Create a logical mask: TRUE = value was imputed
  completed_data$imputed_flag <- is.na(imp$data[,meas])
  
  res <- list(imp = imp, dat = completed_data)
  return(res)
}

# imputes pain and urgency
imp_pain <- impute_data(data = datai %>% select(-urgency), meas = "pain")
imp_urgency <- impute_data(data = datai %>% select(-pain), meas = "urgency")
timp <- list(imp_pain, imp_urgency)
names(timp) <- c("pain", "urgency")

# plotting different comparisons ----

## density
ggplot(timp$pain$dat, aes(pain)) + 
  geom_density(aes(color = "Imputed")) +
  geom_density(aes(color = "Raw"), data = dd) +
  scale_color_manual(
    name = "Data Type",
    values = c("Imputed" = "red", "Raw" = "black")
  )
ggplot(timp$urgency$dat, aes(urgency)) + 
  geom_density(aes(color = "Imputed")) +
  geom_density(aes(color = "Raw"), data = dd) +
  scale_color_manual(
    name = "Data Type",
    values = c("Imputed" = "red", "Raw" = "black")
  )

## boxplot
ggplot(timp$pain$dat, aes(factor(bt_pain), pain)) + 
  geom_boxplot(aes(color = "Imputed"), width = .25) +
  geom_boxplot(data = dd, aes(color = "Raw"), position = position_nudge(x = .3), width = .25) +
  scale_color_manual(
    name = "Data Type",
    values = c("Imputed" = "red", "Raw" = "black")
  )
ggplot(timp$urgency$dat, aes(factor(bt_pain), urgency)) + 
  geom_boxplot(aes(color = "Imputed"), width = .25) +
  geom_boxplot(data = dd, aes(color = "Raw"), position = position_nudge(x = .3), width = .25) +
  scale_color_manual(
    name = "Data Type",
    values = c("Imputed" = "red", "Raw" = "black")
  )

# Comparing imputation using pooled mixed models
library(lme4)

## pain
orig_mod <- lmer(pain ~ 1 + shared + (1 | subject_id), data = datai)
orig_mod2 <- lmer(pain ~ 1 + shared + (1 + shared | subject_id), data = datai)
summary(orig_mod)
summary(orig_mod2)
anova(orig_mod, orig_mod2)

fit <- with(timp$pain$imp, lmer(pain ~ 1 + shared + (1 | subject_id)))
pooled <- pool(fit)
summary(pooled)
summary(orig_mod) # for comparison to non-imputed run

## urgency
orig_mod <- lmer(urgency ~ 1 + shared + (1 | subject_id), data = datai)
orig_mod2 <- lmer(urgency ~ 1 + shared + (1 + shared | subject_id), data = datai)
summary(orig_mod)
summary(orig_mod2)
anova(orig_mod, orig_mod2)

fit <- with(timp$urgency$imp, lmer(urgency ~ 1 + shared + (1 | subject_id)))
pooled <- pool(fit)
summary(pooled)
summary(orig_mod) # for comparison to non-imputed run


# Saving out ----
tj <- c("subject_id", "redcap_event_name", "shared")

## preps pain data
pain_tmp <- 
  timp$pain$dat %>% 
  select(
    subject_id, redcap_event_name, bt_pain, shared, pain, 
    pain_imp = imputed_flag
    )
## preps urgency data
urg_tmp <- 
  timp$urgency$dat %>% 
  select(
    subject_id, redcap_event_name, bt_urgency, shared, urgency, 
    urgency_imp = imputed_flag
  )

# joins all the data and organizes things
dd_res <- 
  dd %>% 
  select(-pain, -urgency) %>%
  full_join(
    ., pain_tmp , by = c("subject_id", "redcap_event_name", "bt_pain", "shared")
    ) %>%
  full_join(
    ., urg_tmp , by = c("subject_id", "redcap_event_name", "bt_urgency", "shared")
  ) %>%
  select(
    subject_id, redcap_event_name, bt_pain, bt_urgency, pain, urgency, pain_imp, 
    urgency_imp
    )

# saves out ----

## data for PCA, etc.
f <- file.path("output", "bladder-task-imp-data.rds")
write_rds(dd_res, file = f) 

## imputation runs
f <- file.path("output", "bladder-task-imp-stats.rds")
write_rds(timp, file = f)

# prepro-cold-pain.R
# Matt Kmiecik
# Started 2025-05-13

# Purpose: prepare the cold pain data for analysis

# libraries ----
library(tidyverse)

# data ----
f <- file.path("output", "EH17338EMPATHY_DATA_2024-11-07_1140_noPHI-joined.csv")
dd <- read_csv(f)

# proc ----

# selecting data
header <- c("record_id", "redcap_event_name", "subject_id")
dd2 <- 
  dd %>% 
  select(all_of(header), "pt_watertemp", "pt4a_cpmwater1", "pt4a1_cpmwater2") 

# plot of the relationship between cold pain and the temp of the water
pj <- position_jitter(height = .1, width = .1)

## 10 seconds
ggplot(dd2, aes(pt_watertemp, pt4a_cpmwater1)) +
  geom_point(shape = 19, alpha = 1/2, position = pj) +
  geom_smooth(method = "lm") +
  labs(
    x = "Water Temperature (°C)", 
    y = "Pain Rating (NRS)", 
    title = "Pain rating taken at 10 seconds."
    ) +
  theme_minimal()

## 20 seconds
ggplot(dd2, aes(pt_watertemp, pt4a1_cpmwater2)) +
  geom_point(shape = 19, alpha = 1/2, position = pj) +
  geom_smooth(method = "lm") +
  labs(
    x = "Water Temperature (°C)", 
    y = "Pain Rating (NRS)", 
    title = "Pain rating taken at 10 seconds."
  ) +
  theme_minimal()

## modeling
library(mgcv)

# 10 seconds
mod1 <- gam(pt4a_cpmwater1 ~ 1 + pt_watertemp, data = dd2, method = "ML")
summary(mod1)
plot(mod1, pages = 1)

mod2 <- gam(pt4a_cpmwater1 ~ 1 + s(pt_watertemp), data = dd2, method = "ML")
summary(mod2)
plot(mod2)

anova(mod1, mod2) # don't trust this given the low edf

# 20 seconds
mod1 <- gam(pt4a1_cpmwater2 ~ 1 + pt_watertemp, data = dd2, method = "ML")
summary(mod1)


mod2 <- gam(pt4a1_cpmwater2 ~ 1 + s(pt_watertemp), data = dd2, method = "ML")
summary(mod2)

# note; given that there was no association between water temp and pain ratings
# it's not necessary to adjust for water temp
events <- c("baseline_visit_chi_arm_1", "visit_1_child_arm_1", "visit2_child_arm_1")
res <- 
  dd2 %>% 
  select(
    subject_id, redcap_event_name, water_temp = pt_watertemp, 
    cold_pain_10s = pt4a_cpmwater1, cold_pain_20s = pt4a1_cpmwater2
    ) %>%
  filter(redcap_event_name %in% events)

# writes out ----
f <- file.path("output", "coldpain-data.rds")
write_rds(res, file = f)

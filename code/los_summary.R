################################################################################
# Analysis of hospital length of stay with COVID-19
# Author: Emily S Nightingale
# Date: 2020-04-15
################################################################################
#
# This script created the figures presented in 
# "COVID-19 length of hospital stay: a systematic review and data synthesis"
# 
################################################################################
################################################################################


# ---------------------------------------------------------------------------- #
# Load packages
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(linelist)

source(here::here("code","plot_function.R"))

# ---------------------------------------------------------------------------- #
# Data set up
# ---------------------------------------------------------------------------- #

dat <- read.csv(here::here("data","LOS analysis dataset.csv"))
# names(dat)


los <- dat %>%
  select(StudyNo, Author, Title, date_published, date_first_admit, date_last_admit, Country, 
         All.patients.discharged..dead.,age_group, covid_severity, grouped_by_severity, 
         other_feature_comorbidity, trt_group, other_group, outcome, los_group, plot_cat, N, 
         perc_male, age_med, age_mean, age_specific_for_group, plot_cat, 
         LOS_med, LOS_q25, LOS_q75, LOS_mean, LOS_sd, LOS_min, LOS_max, 
         LOS_nonICU_med, LOS_nonICU_q25, LOS_nonICU_q75, 
         LOS_ICU_med, LOS_ICU_q25,LOS_ICU_q75, LOS_ICU_mean, LOS_ICU_sd) %>%
  mutate_at(vars(date_published:date_last_admit),
            guess_dates,
            error_tolerance = 1) %>%
  mutate(
    complete_fup = factor(All.patients.discharged..dead., levels = c("Yes","No")),
    outcome = factor(outcome, levels = c("All", "Alive", "Dead")),
    plot_oth_group = paste(outcome, other_group, sep = ":"),
    avg_age = case_when(!is.na(age_med) ~ age_med,
                        is.na(age_med) ~ age_mean))


# Copy down Study info to all rows
study_vars <- c("StudyNo","Author","Title","date_published","date_first_admit", "date_last_admit", "Country")
for (i in 1:nrow(los)){
  if (los$Title[i] != ""){study_info <- los[i, study_vars]
  }else{los[i, study_vars] <-  study_info}
}


# Define study date: first admission if available, otherwise last admission
los$study_date <- los$date_first_admit
los$study_date[is.na(los$study_date)] <- los$date_last_admit[is.na(los$study_date)]
los$study_date[is.na(los$study_date)] <- los$date_published[is.na(los$study_date)]


# Define study identifier: concatenate author with study date
los <- 
  los %>%
  mutate(Study = as.factor(trimws(sprintf("%s (%s)",
                                          Author, 
                                          study_date))),
         Setting = case_when(Country == "China" ~ "China",
                             Country != "China" ~ "Other")) %>%
  select(Study, everything()) 


# Pad study identifier for plotting consistency
los$Study <- str_pad(los$Study, width = max(str_length(los$Study)), side = "left")
n_distinct(los$Study)


# ---------------------------------------------------------------------------- #
# Overall summary
# ---------------------------------------------------------------------------- #


# Summarise patient characteristics - age
los %>%
  filter(age_group != "pediatric") -> adult
wt.mean(adult$avg_age, wt = adult$N) # 59.33595
wt.sd(adult$avg_age, wt = adult$N) # 9.646322
wt.mean(adult$perc_male, wt = adult$N) # 56.2328
wt.sd(adult$perc_male, wt = adult$N) #  10.93115


los %>%
  filter(age_group == "pediatric") -> ped
wt.mean(ped$avg_age, wt = ped$N) # 6.933
wt.sd(ped$avg_age, wt = ped$N) # 2.805726


# Identify which estimates are reported as mean/median
los <- 
  los %>%
  mutate(metric = case_when((!is.na(LOS_mean) | !is.na(LOS_ICU_mean)) ~ "mean",
                            (!is.na(LOS_med) | !is.na(LOS_nonICU_med) | !is.na(LOS_ICU_med)) ~ "median")) %>%
  filter(!is.na(metric)) # filter out study summary rows without any LOS value


# Split out general LOS from ICU
los_gen <- los %>%
  filter(los_group %in% c("general", "general/ICU","non-ICU")) %>%
  select(-LOS_ICU_med:-LOS_ICU_sd) %>% 
  mutate(LOS_avg = LOS_med)


los_icu <- los %>%
  filter(los_group %in% c("ICU", "general/ICU"))  %>%
  select(-LOS_med:-LOS_max) %>%
  mutate(LOS_avg = LOS_ICU_med,
         LOS_q25 = LOS_ICU_q25,
         LOS_q75 = LOS_ICU_q75) 


# ---------------------------------------------------------------------------- #
# LOS General
# ---------------------------------------------------------------------------- #

# Function to calculate weibull quantiles from mean/sd
get_weib_quants <- function(input, meanvar, sdvar){
  
    parms <- mixdist::weibullpar(mu = input[meanvar], sigma = input[sdvar])
    dist <- distcrete::distcrete(name = "weibull",
                                 interval = 1, 
                                 w = 0.5,
                                 shape = parms$shape, 
                                 scale = parms$scale) 
    quants <- dist$q(c(0.5,0.25,0.75))

  return(quants)
}


# Calculate weibull quantiles for rows with mean/sd
for (i in 1:nrow(los_gen)){
  if (los_gen$metric[i] == "mean"){
    los_gen$LOS_avg[i] <- los_gen$LOS_mean[i]
    if(!is.na(los_gen$LOS_sd[i])){
      los_gen[i,c("LOS_avg","LOS_q25","LOS_q75")] <- get_weib_quants(los_gen[i,], "LOS_mean", "LOS_sd")
      los_gen$metric[i] <- "median (derived)"
  }}
}


# Add nonICU med/IQRs (no means reported for nonICU stay) 
los_nonICU <- 
  los_gen %>%
  filter(!is.na(LOS_nonICU_med)) %>%
  mutate(LOS_avg = LOS_nonICU_med,
         LOS_q25 =  LOS_nonICU_q25,
         LOS_q75 =  LOS_nonICU_q75) 


los_gen <- bind_rows(los_gen, los_nonICU) 


# los_avgs %>%
#   filter(outcome == "Alive") %>%
#   View()
# 
# los_avgs %>%
#   filter(outcome == "Dead") %>%
#   View()


# If plot_cat = "main", rows are unique by study and outcome
los_gen %>%
  filter(plot_cat == "main") -> main


# Figure 2: main estimates from each study for general LOS, by discharge status. 
# (i.e. excluding specific severity/comorbidity/treatment subgroups).
png(filename = here::here("figures","fig2_outcomes_bysetting.png"), height = 1800, width = 1500, res = 150)
main %>%
  plot_los_outcome(col = "outcome") +  
  labs(col = "Discharge status", 
       title = "Length of stay in hospital, by discharge status") 
dev.off()


# Supplementary figure: Ordered by median age
# Add avg age to study ID and highlight where not specific for group: 
main %>%
  filter(!is.na(avg_age)) %>%# 
  mutate(Study = sprintf("%s (%s)      %s",
                         Author, 
                         study_date,
                         round(avg_age))) -> by_age
by_age$Study[by_age$age_specific_for_group == "N"] <- paste0(by_age$Study[by_age$age_specific_for_group == "N"],"*")


png(filename = here::here("figures","fig2b_all_outcomes_bymedage.png"), height = 1500, width = 1500, res = 150)
by_age %>%
  mutate(Study = as.factor(Study)) %>%
  plot_los_outcome_nofacet(order = "avg_age", col = "outcome") +
  labs(col = "Discharge status", 
       title = "Length of stay in hospital, by discharge status",
       subtitle = "Studies ordered by average participant age (given on the vertical axis)")
dev.off()

# Supplementary figure: General/ICU LOS estimates, where reported for specific 
# disease severity subgroups.
png(filename = here::here("figures","figSup_all_outcomes_byseverity.png"), height = 1300, width = 1500, res = 150)
los_gen %>%
  filter(grouped_by_severity == "Y" & trt_group == "" & covid_severity != "All") %>%
  plot_los_outcome_nofacet(plot_outcome = NULL, col = "covid_severity", group = "covid_severity") +
  labs(col = "Disease severity",
       title = "Length of stay in hospital, by study-specified disease severity")
dev.off()



# ---------------------------------------------------------------------------- #
# LOS ICU
# ---------------------------------------------------------------------------- #

# Calculate weibull quantiles for rows with mean/sd
for (i in 1:nrow(los_icu)){
  if (los_icu$metric[i] == "mean"){
    los_icu$LOS_avg[i] <- los_icu$LOS_ICU_mean[i]
    if(!is.na(los_icu$LOS_ICU_sd[i])){
      los_icu[i,c("LOS_avg","LOS_q25","LOS_q75")] <- get_weib_quants(los_icu[i,], "LOS_ICU_mean","LOS_ICU_sd")
      los_icu$metric[i] <- "median (derived)"
    }}
}


# Figure 3: main estimates from each study for ICU LOS, by discharge status. 
# (i.e. excluding specific severity/comorbidity/treatment subgroups).
png(filename = here::here("figures","fig3_icu_outcomes_bysetting.png"), height = 1000, width = 1500, res = 150)
los_icu %>%
  filter(trt_group == "") %>%
  plot_los_outcome(plot_outcome = NULL, col = "outcome") +
  facet_grid(rows = vars(Setting), scales = "free_y") + 
  labs(col = "Discharge status",
       title = "Length of stay in ICU, by discharge status")
dev.off()


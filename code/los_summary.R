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

dat <- read.csv(here::here("data","LOS analysis dataset.csv"),
                stringsAsFactors = F)
# names(dat)


los <- dat %>%
  dplyr::select(StudyNo, Author, Title, date_published, date_first_admit, date_last_admit, Country, 
                All.patients.discharged..dead.,age_group, covid_severity, grouped_by_severity, 
                other_feature_comorbidity, trt_group, other_group, outcome, los_group, plot_cat, N, 
                perc_male, age_med, age_mean, age_specific_for_group, plot_cat, 
                LOS_med, LOS_q25, LOS_q75, LOS_mean, LOS_sd, LOS_min, LOS_max, 
                LOS_nonICU_med, LOS_nonICU_q25, LOS_nonICU_q75, 
                LOS_ICU_med, LOS_ICU_q25,LOS_ICU_q75, LOS_ICU_mean, LOS_ICU_sd) %>%
  dplyr::mutate_at(vars(date_published:date_last_admit),
                   guess_dates,
                   error_tolerance = 1) %>%
  dplyr::mutate(
    complete_fup   = factor(All.patients.discharged..dead., levels = c("Yes","No")),
    outcome        = factor(outcome, levels = c("All", "Alive", "Dead")),
    plot_oth_group = paste(outcome, other_group, sep = ":"),
    avg_age        = dplyr::case_when(!is.na(age_med) ~ age_med,
                                      is.na(age_med) ~ age_mean)) %>%
  dplyr::mutate(Country = if_else(Country == "US", "USA", Country))


# Copy down Study info to all rows
study_vars <- c("StudyNo", "Author", "Title",
                "date_published", "date_first_admit", "date_last_admit", 
                "Country")




los <- tidyr::fill(los, .direction = "down", StudyNo) %>%
  split(.$StudyNo) %>%
  purrr::map(.f = ~dplyr::mutate_at(.x,
                                    .vars = tidyselect::all_of(study_vars),
                                    .funs = function(x) sub(pattern = "^$", replacement = NA, x = x)))  %>%
  purrr::map_df(.f= ~tidyr::fill(.x, .direction = "down", tidyselect::all_of(study_vars)))

# Define study date: first admission if available, otherwise last admission
los <- dplyr::mutate(los, 
                     study_date = dplyr::case_when(
                       !is.na(date_first_admit) ~ date_first_admit,
                       !is.na(date_last_admit)  ~ date_last_admit,
                       !is.na(date_published)   ~ date_published))

# Define study identifier: concatenate author with study date
los <- 
  los %>%
  dplyr::mutate(Study = as.factor(trimws(sprintf("%s (%s)",
                                                 Author, 
                                                 study_date))),
                Setting = dplyr::case_when(Country == "China" ~ "China",
                                           Country != "China" ~ "Other")) %>%
  dplyr::select(Study, dplyr::everything()) 


dplyr::n_distinct(los$Study)


# ---------------------------------------------------------------------------- #
# Overall summary
# ---------------------------------------------------------------------------- #


# Summarise patient characteristics - age
los %>%
  dplyr::filter(age_group != "pediatric") -> adult

# wt.mean from SDMtools
wt.summary <- function(x){
  dplyr::summarise_at(x, .vars = vars(avg_age, perc_male), 
                      list(mean = ~wt.mean(., wt = N),
                           sd   = ~wt.sd(., wt = N),
                           n    = ~length(na.omit(.)))) %>%
    tidyr::gather(key, value) %>%
    transform(., variable = sub("(.*)_.*", "\\1", key), summary = sub(".*_", "", key)) %>%
    dplyr::arrange(variable, summary) %>%
    dplyr::select(variable, summary, tidyselect::everything(), value, -key) }

wt.summary(los)  

los %>%
  split(.$age_group) %>%
  purrr::map_df(~wt.summary(.x), .id = "age_group")


# Identify which estimates are reported as mean/median
los <- 
  los %>%
  dplyr::mutate(metric = dplyr::case_when((!is.na(LOS_mean) | !is.na(LOS_ICU_mean)) ~ "mean",
                                          (!is.na(LOS_med) | !is.na(LOS_nonICU_med) | !is.na(LOS_ICU_med)) ~ "median")) %>%
  dplyr::filter(!is.na(metric)) # filter out study summary rows without any LOS value


# Split out general LOS from ICU
los_gen <- los %>%
  dplyr::filter(los_group %in% c("general", "general/ICU","non-ICU")) %>%
  dplyr::select(-LOS_ICU_med:-LOS_ICU_sd) %>% 
  dplyr::mutate(LOS_avg = LOS_med)


los_icu <- los %>%
  dplyr::filter(los_group %in% c("ICU", "general/ICU"))  %>%
  dplyr::select(-LOS_med:-LOS_max) %>%
  dplyr::mutate(LOS_avg = LOS_ICU_med,
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

ggplot2::ggsave(filename = here::here("figures","fig2_outcomes_bysetting.png"),
                height = 12, width = 10, dpi = 150, units = "in",
                plot = {
                  plot_los_outcome(main, col = "outcome") +  
                    labs(col = "Discharge status", 
                         title = "Length of stay in hospital, by discharge status")}, )



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


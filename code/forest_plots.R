################################################################################
# Analysis of hospital length of stay with COVID-19
# Author: Emily S Nightingale
# Date: 2020-04-15
################################################################################
#
# This script performs the analysis presented in "PAPER NAME"
# 
################################################################################
################################################################################


# ---------------------------------------------------------------------------- #
# Load packages
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(linelist)

# ---------------------------------------------------------------------------- #
# Data set up
# ---------------------------------------------------------------------------- #

dat <- read.csv("~/COVID-19/los review/LOS analysis dataset.csv")

los <- dat %>%
  select(Author, Title, date_published, date_first_admit, date_last_admit, Country, 
         Province, City, All.patients.discharged..dead.,  age_group, covid_severity, grouped_by_severity, 
         other_feature_comorbidity, trt_group, outcome, N, 
         perc_male, age_med, plot_cat, 
         LOS_med, LOS_q25, LOS_q75, LOS_mean, LOS_sd, LOS_min, LOS_max, 
         LOS_nonICU_med, LOS_nonICU_q25, LOS_nonICU_q75, 
         LOS_ICU_med, LOS_ICU_q25,LOS_ICU_q75, LOS_ICU_mean, LOS_ICU_sd) %>%
  mutate_at(vars(date_published:date_last_admit),
            guess_dates,
            error_tolerance = 1) %>%
  mutate(
    complete_fup = factor(All.patients.discharged..dead., levels = c("Yes","No")),
    outcome = factor(outcome, levels = c("Discharge or death", "Discharge", "Death")))

# Copy down Study info
study_vars <- c("Author","Title","date_published","date_first_admit", "date_last_admit", "Country", "Province", "City")
for (i in 1:nrow(los)){
  if (los$Title[i] != ""){study_info <- los[i, study_vars]
  }else{los[i, study_vars] <-  study_info}
}

# Define study date: first admission if available, otherwise last admission
los$study_date <- los$date_first_admit
los$study_date[is.na(los$study_date)] <- los$date_last_admit[is.na(los$study_date)]
los$study_date[is.na(los$study_date)] <- los$date_published[is.na(los$study_date)]


# Define study identifier: Concatenate author with study date
los <- 
  los %>%
  mutate(Study = as.factor(sprintf("%s (%s)",
                                   Author, 
                                   study_date)),
         Setting = case_when(Country == "China" ~ "China",
                             Country != "China" ~ "Other")) %>%
  select(Study, everything())


# Split out general from ICU LOS
los_gen <- los %>%
   filter(!is.na(LOS_med) | !is.na(LOS_mean) | !is.na(LOS_nonICU_med)) 
los_icu <- los %>%
  filter(!is.na(LOS_ICU_med) | !is.na(LOS_ICU_mean)) 

# ------------------------- Plotting function -------------------------------- #

pd <- position_dodge(width = 0.5)
plot_los_outcome <- function(data, plot_outcome = NULL, order = "study_date", lty = "complete_fup", col = "covid_severity"){
  
  data <- mutate(data, Study = fct_reorder(Study, !!sym(order)))
  
  if (!is.null(plot_outcome)){
    data <- filter(data, outcome == plot_outcome)
  }
  
  data %>%
    ggplot(aes(y = Study, x = LOS_avg, xmin = LOS_lower, xmax = LOS_upper, shape = metric, lty = !!sym(lty), col = !!sym(col))) +
    geom_point(aes(size = N), position=pd) +
    # geom_segment(aes(x = LOS_upper - 0.1, xend = LOS_upper, yend = Study),
    #                  position=pd,
    #                  lty = "solid",
    #                  arrow = arrow(length = unit(0.01, "npc"))) +
    geom_errorbarh(height=0.3, position = pd) +
    # geom_boxplot(aes(middle = LOS_avg, xlower = LOS_lower, xupper = LOS_upper, xmin = LOS_min, xmax = LOS_max)) +
    geom_vline(aes(xintercept = median(LOS_avg)), col = "darkgrey", lty = "dashed") +
    labs(x = "Length of stay (days)",
         title = "Length of stay in hospital: all outcomes (incl. survivors, non-survivors and mixed)",
         caption = paste0("Studies ordered by ", order),
         size = "Study size",
         lty = "Follow-up complete?",
         shape = "Measure",
         col = "Disease severity") +
    scale_color_brewer(palette = "Dark2") +
    theme_minimal()
}


# ---------------------------------------------------------------------------- #
# LOS: general/unspecified
# ---------------------------------------------------------------------------- #

# Identify mean/median LOS
los_med <- 
  los_gen %>%
  filter(!is.na(LOS_med)) %>%
  mutate(LOS_avg = LOS_med,
         LOS_lower = LOS_q25,
         LOS_upper = LOS_q75,
         metric = "median") 

los_mean <- 
  los_gen %>%
  filter(!is.na(LOS_mean)) %>%
  mutate(LOS_avg = LOS_mean,
         LOS_lower =  LOS_mean + qnorm(0.25)*LOS_sd,
         LOS_upper =  LOS_mean + qnorm(0.75)*LOS_sd,
         metric = "mean") 

los_nonICU <- 
  los_gen %>%
  filter(!is.na(LOS_nonICU_med)) %>%
  mutate(LOS_avg = LOS_nonICU_med,
         LOS_lower =  LOS_nonICU_q25,
         LOS_upper =  LOS_nonICU_q75,
         metric = "median") 

los_avgs <- bind_rows(los_med, los_mean, los_nonICU) %>%
  dplyr::select(-LOS_med:-LOS_sd)



# If plot_cat = "main", should be unique by study and outcome
los_avgs %>%
  filter(plot_cat == "main") -> main


# ------------------------------ Plot LOS ------------------------------------ #


# Figure 2: main estimates from each study for general LOS, by discharge status. 
# (i.e. excluding specific severity/comorbidity/treatment subgroups).
png(filename = "~/COVID-19/los review/figures/fig1_all_outcomes.png", height = 1800, width = 1500, res = 150)
main %>%
  # filter(trt_group == "") %>%
  plot_los_outcome(col = "outcome") +  
  labs(col = "Discharge status", title = "Length of stay in hospital, by discharge status")
dev.off()
# 
# png(filename = "~/COVID-19/los review/figures/non_survivors.png", height = 1000, width = 1500, res = 150)
# all %>%
#   plot_los_outcome(plot_outcome = "Death") +  
#   labs(title = "Length of stay in hospital: non-survivors")
# dev.off()
# 
# png(filename = "~/COVID-19/los review/figures/survivors.png", height = 1300, width = 1500, res = 150)
# all %>%
#   plot_los_outcome(plot_outcome = "Discharge") +  
#   labs(title = "Length of stay in hospital: Survivors")
# dev.off()

# # Ordered by median age
# png(filename = "~/COVID-19/los review/figures/all_outcomes_bymedage.png", height = 1500, width = 1500, res = 150)
# main %>%
#   plot_los_outcome(order = "age_med", col = "outcome") +  
#   labs(col = "Discharge status")
# dev.off()
# 
# png(filename = "~/COVID-19/los review/figures/all_outcomes_ped.png", height = 900, width = 1500, res = 150)
# all %>%
#   filter(age_group == "pediatric") %>%
#   plot_los_outcome(plot_outcome = NULL, col = "covid_severity") +
#   labs(col = "Disease severity",
#        title = "Length of stay in hospital, by disease severity: Pediatric studies")
# dev.off()
# 
# 
# png(filename = "~/COVID-19/los review/figures/all_outcomes_othersg.png", height = 900, width = 1500, res = 150)
# los_avgs %>%
#   filter(other_feature_comorbidity != "") %>%
#   plot_los_outcome(plot_outcome = NULL, col = "other_feature_comorbidity") +
#   scale_colour_discrete() +
#   labs(col = "Subgroup/comorbidity",
#        title = "Length of stay in hospital: other disease subgroups")
# dev.off()


##############################################################################
##############################################################################

# Identify mean/median LOS for ICU stay
los_icu_med <- 
  los_icu %>%
  filter(!is.na(LOS_ICU_med)) %>%
  mutate(LOS_avg = LOS_ICU_med,
         LOS_lower = LOS_ICU_q25,
         LOS_upper = LOS_ICU_q75,
         metric = "median") 


los_icu_mean <- 
  los_icu %>%
  filter(!is.na(LOS_ICU_mean)) %>%
  mutate(LOS_avg = LOS_ICU_mean,
         LOS_lower =  LOS_ICU_mean + qnorm(0.25)*LOS_ICU_sd,
         LOS_upper =  LOS_ICU_mean + qnorm(0.75)*LOS_ICU_sd,
         metric = "mean") 

los_icu_avgs <- bind_rows(los_icu_mean, los_icu_med)


# Figure 3: main estimates from each study for ICU LOS, by discharge status. 
# (i.e. excluding specific severity/comorbidity/treatment subgroups).
png(filename = "~/COVID-19/los review/figures/icu_by_outcome.png", height = 1000, width = 1500, res = 150)
los_icu_avgs %>%
  filter(trt_group == "") %>%
  plot_los_outcome(plot_outcome = NULL, col = "outcome") +
  labs(col = "Outcome",
       title = "Length of stay in ICU, by discharge status")
dev.off()


# Supplementary figure: General/ICU LOS estimates, where reported for specific 
# disease severity subgroups.
png(filename = "~/COVID-19/los review/figures/figSup_all_outcomes_byseverity.png", height = 1300, width = 1500, res = 150)
los_avgs %>%
  bind_rows(los_icu_avgs) %>%
  filter(grouped_by_severity == "Y" & trt_group == "" & covid_severity != "All") %>%
  plot_los_outcome(plot_outcome = NULL, col = "covid_severity") +
  facet_grid(scales = "free", rows = vars(outcome)) +
  labs(col = "Disease severity",
       title = "Length of stay in hospital/ICU, by study-specified disease severity")
dev.off()



# ---------------------------------------------------------------------------- #
# Are means substantially different to medians?

los_avgs %>%
  group_by(metric) %>%
  summarise(summary_median = median(LOS_avg),
            summary_mean = mean(LOS_avg),
            summary_sd = sqrt(var(LOS_avg)))
# # A tibble: 2 x 4
#   metric summary_median summary_mean summary_sd
#   <chr>           <dbl>        <dbl>      <dbl>
# 1 mean             13.4         12.8       3.58
# 2 median           14           15.7       7.38



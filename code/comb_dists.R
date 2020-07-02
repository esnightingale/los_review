################################################################################
# Analysis of hospital length of stay with COVID-19
# Author: Naomi R Waterlow
# Date: 2020-04-29
################################################################################
#
# This script performs the analysis of estimation of LOS
#
################################################################################
################################################################################

# init_values for the fitting must be in right area to get a good fit.
# sample_size is for sampling th overall distribution at the end.

library(here)

# setwd("~/Documents/GitHub/los_review/code/")
# Load the functions
source(here::here("code","comb_dist_funcs.R"))
#Load and format the data
source(here::here("code","comb_dist_data.R"))

###### INPUT ######

# interquatile range. Can change to something else but have to do equivalent in qunatiles
iqr <- c(0.25,0.5,0.75)
sample_size <- 100000 # sample_size is for sampling th overall, combined distribution
set.seed(643)

####### GENERAL ########

# run the function to create an overall distribution for los in China, General Hopsital
general_samples_china <- create_dist_weibull_discrete(los_general_china_s,
                                                        sample_size = sample_size,
                                                        init_values = c(3,27))
# for los in rest of world, General Hopsital
general_samples_world <- create_dist_weibull_discrete(los_general_world_s,
                                                      sample_size = sample_size,
                                                      init_values = c(3,27))


######## ICU ######

# rename columns so can use the amin function
colnames(los_icu_s) <- c("N", "LOS_med", "LOS_q25", "LOS_q75", "LOS_mean", "LOS_sd")
colnames(los_icu_china_s) <- c("N", "LOS_med", "LOS_q25", "LOS_q75", "LOS_mean", "LOS_sd")
colnames(los_icu_world_s) <- c("N", "LOS_med", "LOS_q25", "LOS_q75", "LOS_mean", "LOS_sd")

# Get samples for ICU china
icu_samples_china <- create_dist_weibull_discrete(los_icu_china_s,
                                                sample_size = sample_size,
                                                init_values = c(3,27))

# Get samples for ICU rest of the world
icu_samples_world <- create_dist_weibull_discrete(los_icu_world_s,
                                                  sample_size = sample_size,
                                                  init_values = c(3,27))


##### CREATE SUMMARY ANALYSIS #######

# Create a histogram for the different sub_groups
HIST_PLOT <- plot_hist_1(icu_china = icu_samples_china[["samples"]],
                       icu_world = icu_samples_world[["samples"]],
                       general_china = general_samples_china[["samples"]],
                       general_world = general_samples_world[["samples"]])

pdf(here::here("figures","histograms.pdf"))
HIST_PLOT
dev.off()

# Calcualte the quantiles for each subgroup
quants_china_general <- quantile(general_samples_china[["samples"]], probs=iqr)
quants_china_icu <- quantile(icu_samples_china[["samples"]], probs=iqr)
quants_world_general <- quantile(general_samples_world[["samples"]],  probs=iqr)
quants_world_icu <- quantile(icu_samples_world[["samples"]],probs=iqr)

# View the % of samples that fall above the 60 xaxis cut off
china_general_over_60 <- sum(general_samples_china[["samples"]] > 60) /
  length(general_samples_china[["samples"]])*100
china_icu_over_60 <- sum(icu_samples_china[["samples"]] > 60) /
  length(icu_samples_china[["samples"]])*100
world_icu_over_60 <- sum(icu_samples_world[["samples"]] > 60) /
  length(icu_samples_world[["samples"]])*100
  world_general_over_60 <- sum(general_samples_world[["samples"]] > 60) /
  length(general_samples_world[["samples"]])*100


####### COMPARE COMPLETE VS ONGOING STUDIES #######

# Generate samples only from distributions from studies where not everonye has been discharged
general_samples_china_ongoing <- create_dist_weibull_discrete(los_general_china_ongoing_s,
                                                      sample_size = sample_size,
                                                      init_values = c(3,27))

# Generate samples only from distributions from studies where everyone has been discharge
general_samples_china_complete <- create_dist_weibull_discrete(los_general_china_complete_s,
                                                              sample_size = sample_size,
                                                              init_values = c(3,27))


HIST_PLOT_2 <- plot_hist_2(china_ongoing = general_samples_china_ongoing[["samples"]],
                         china_complete = general_samples_china_complete[["samples"]])

pdf(here::here("figures","histograms_complete.pdf"))
HIST_PLOT_2
dev.off()

quants_china_ongoing <- quantile(general_samples_china_ongoing[["samples"]], probs=iqr)
quants_china_complete <- quantile(general_samples_china_complete[["samples"]], probs=iqr)
china_complete_over_60 <- sum(general_samples_china_complete[["samples"]] > 60) /
  length(general_samples_china_complete[["samples"]])*100
china_ongoing_over_60 <- sum(general_samples_china_ongoing[["samples"]] > 60) /
  length(general_samples_china_ongoing[["samples"]])*100


# ###### CALCULATE ERRRORS #######

# Extract errors from weibull (general, china)
weibull_errors <- general_samples_china[["errors"]]
# Calculate equivalent gamma errors
 gamma_errors <-  errors_gamma(los_general_china_s,
                                               sample_size = sample_size,
                                               init_values = c(3,27))
 # combine the errors into a dataframe
 all_errors <- data.frame(errors = c(weibull_errors, gamma_errors),
                          type = c(rep("weibull", length(weibull_errors)),
                                   rep("gamma", length(gamma_errors))))
ERROR_PLOT <- ggplot(all_errors, aes(x=errors)) +
   geom_histogram(bins=10) +
   facet_grid(~type) + theme_bw()

 # save error plot
pdf(here::here("figures","error_plot.pdf"))
ERROR_PLOT
dev.off()
# Total error in each case
sum(gamma_errors)
sum(weibull_errors)


########## General analysis - no weightings ########

#Rerun all the sample generations but with weighiting = False
general_samples_china_2 <- create_dist_weibull_discrete(los_general_china_s,
                                                      sample_size = sample_size,
                                                      init_values = c(3,27),
                                                      weighting = F)

general_samples_world_2 <- create_dist_weibull_discrete(los_general_world_s,
                                                      sample_size = sample_size,
                                                      init_values = c(3,27),
                                                      weighting = F)


icu_samples_china_2 <- create_dist_weibull_discrete(los_icu_china_s,
                                                  sample_size = sample_size,
                                                  init_values = c(3,27),
                                                  weighting = F)

icu_samples_world_2 <- create_dist_weibull_discrete(los_icu_world_s,
                                                  sample_size = sample_size,
                                                  init_values = c(3,27),
                                                  weighting = F)
# Create histogram of all the unweighted samplings
HIST_PLOT_NoWeight <- plot_hist_1(icu_china = icu_samples_china_2[["samples"]],
                         icu_world = icu_samples_world_2[["samples"]],
                         general_china = general_samples_china_2[["samples"]],
                         general_world = general_samples_world_2[["samples"]])

pdf(here::here("figures","histograms_no_weight.pdf"))
HIST_PLOT_NoWeight
dev.off()

# Calcualte the quantiles
quants_china_general_2 <- quantile(general_samples_china_2[["samples"]], probs=iqr)
quants_china_icu_2 <- quantile(icu_samples_china_2[["samples"]], probs=iqr)
quants_world_general_2 <- quantile(general_samples_world_2[["samples"]],  probs=iqr)
quants_world_icu_2 <- quantile(icu_samples_world_2[["samples"]],probs=iqr)


#### Compare weighted vs non_weighted


#Combine into dataframes for plotting
icu_china_w <- data.frame(samples =icu_samples_china[["samples"]], location = "China", type = "ICU", weighted = "yes")
icu_world_w <- data.frame(samples =icu_samples_world[["samples"]], location = "Rest of World", type = "ICU", weighted = "yes")
general_china_w <- data.frame(samples =general_samples_china[["samples"]], location = "China", type = "General", weighted = "yes")
general_world_w <- data.frame(samples =general_samples_world[["samples"]], location = "Rest of World", type = "General", weighted = "yes")

icu_china_nw <- data.frame(samples =icu_samples_china_2[["samples"]], location = "China", type = "ICU", weighted = "no")
icu_world_nw <- data.frame(samples =icu_samples_world_2[["samples"]], location = "Rest of World", type = "ICU", weighted = "no")
general_china_nw <- data.frame(samples =general_samples_china_2[["samples"]], location = "China", type = "General", weighted = "no")
general_world_nw <- data.frame(samples =general_samples_world_2[["samples"]], location = "Rest of World", type = "General", weighted = "no")

all_samples_weighted <- rbind(icu_china_w, icu_world_w, general_china_w, general_world_w)
all_samples_unweighted <- rbind(icu_china_nw, icu_world_nw, general_china_nw, general_world_nw)

# Plot a comparison between samples generated based on weightings and not.
COMPARISON_PLOT <- ggplot(all_samples_weighted, aes(x=samples), colour = "darkgrey",
                          fill = "darkgrey") +
  geom_histogram(bins=61)+
  facet_grid(location~type) + theme_bw() +
  scale_x_continuous(breaks = seq(0, 60, by = 5), limits=c(0,60)) +
  labs(x ="Length of Stay (days)", y="Counts") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  geom_histogram(data=all_samples_unweighted,bins=61,aes(x=samples, alpha =0.1),
                 fill = "#FF6B94", alpha= 0.6) +
  scale_colour_manual(values= alpha(c("#FF6B94"), 0.6))
COMPARISON_PLOT

pdf(here::here("figures","Comparison_weighted.pdf"))
COMPARISON_PLOT
dev.off()

write.csv( general_samples_china[["samples"]], "distribution_general_china.csv")
write.csv( icu_samples_china[["samples"]],"distribution_icu_china.csv")
write.csv( general_samples_world[["samples"]], "distribution_general_world.csv")
write.csv(icu_samples_world[["samples"]], "distribution_icu_world.csv" )


### Summary by discharge status ### 

# run the function to create an overall distribution for los in patients who survived, General Hopsital
general_samples_surv <- create_dist_weibull_discrete(dplyr::select(los_general_china_surv, N, LOS_med:LOS_sd),
                                                      sample_size = sample_size,
                                                      init_values = c(3,27))
# for los in those who died, General Hopsital
general_samples_dead <- create_dist_weibull_discrete(dplyr::select(los_general_china_dead, N, LOS_med:LOS_sd),
                                                      sample_size = sample_size,
                                                      init_values = c(3,27))

all_samples_status <- rbind(data.frame(samples =general_samples_surv[["samples"]], status = "Discharged alive"),
                            data.frame(samples =general_samples_dead[["samples"]], status = "Died"))

# Create a histogram for the different sub_groups
HIST_PLOT_STATUS <- ggplot(all_samples_status, aes(x=samples), colour = "darkgrey",
                          fill = "darkgrey") +
  geom_histogram(bins=61)+
  facet_grid(~status) + theme_bw() +
  scale_x_continuous(breaks = seq(0, 60, by = 5), limits=c(0,60)) +
  labs(x ="Length of Stay (days)", y="Counts") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  geom_histogram(data=all_samples_status,bins=61,aes(x=samples, alpha =0.1),
                 fill = "#FF6B94", alpha= 0.6) +
  scale_colour_manual(values= alpha(c("#FF6B94"), 0.6))
HIST_PLOT_STATUS

pdf(here::here("figures","dischargestatus.pdf"), width = 8, height = 4)
HIST_PLOT_STATUS
dev.off()

png(here::here("figures","dischargestatus.png"), width = 600, height = 300)
HIST_PLOT_STATUS
dev.off()

# Calcualte the quantiles for each subgroup
quants_surv_general <- quantile(general_samples_surv[["samples"]], probs=iqr)
quants_died_general <- quantile(general_samples_died[["samples"]], probs=iqr)

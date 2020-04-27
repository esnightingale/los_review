# Fits a weibull distribution to the iqr range for each input set, then samples them
# propotionaly based on the data set size. 
# Run the function to create the overall distribution.
# init_values for the fitting must be in right area to get a good fit.
# sample_size is for sampling th overall distribution at the end.

# Load in the functions
setwd("~/Documents/GitHub/los_review/code/")
source("comb_dist_funcs.R")
source("comb_dist_data.R")
###### INPUT ######

# interquatile range. Can change to something else but have to do equivalent in qunatiles
iqr <- c(0.25,0.5,0.75)
sample_size <- 100000
set.seed(643)

####### GENERAL ########

#printed values describe error in fit. Ideally less than 0.001
general_samples_china <- create_dist_weibull_discrete(los_general_china_s,
                                                        sizes, 
                                                        sample_size = sample_size, 
                                                        init_values = c(3,27))

general_samples_world <- create_dist_weibull_discrete(los_general_world_s,
                                                      sizes, 
                                                      sample_size = sample_size, 
                                                      init_values = c(3,27))


######## ICU ######

#printed values describe error in fit. Ideally less than 0.001
colnames(los_icu_s) <- c("N", "LOS_med", "LOS_q25", "LOS_q75", "LOS_mean", "LOS_sd")
colnames(los_icu_china_s) <- c("N", "LOS_med", "LOS_q25", "LOS_q75", "LOS_mean", "LOS_sd")
colnames(los_icu_world_s) <- c("N", "LOS_med", "LOS_q25", "LOS_q75", "LOS_mean", "LOS_sd")


icu_samples_china <- create_dist_weibull_discrete(los_icu_china_s,
                                                sizes, 
                                                sample_size = sample_size, 
                                                init_values = c(3,27))

icu_samples_world <- create_dist_weibull_discrete(los_icu_world_s,
                                                  sizes, 
                                                  sample_size = sample_size, 
                                                  init_values = c(3,27))


##### CREATE SUMMARY ANALYSIS #######

HIST_PLOT <- plot_hist_1(icu_china = icu_samples_china[["samples"]], 
                       icu_world = icu_samples_world[["samples"]], 
                       general_china = general_samples_china[["samples"]],
                       general_world = general_samples_world[["samples"]])

pdf("histograms.pdf")
HIST_PLOT
dev.off()

#quantiles
quants_china_general <- quantile(general_samples_china[["samples"]], probs=iqr)
quants_china_icu <- quantile(icu_samples_china[["samples"]], probs=iqr)
quants_world_general <- quantile(general_samples_world[["samples"]],  probs=iqr)
quants_world_icu <- quantile(icu_samples_world[["samples"]],probs=iqr)

china_general_over_60 <- sum(general_samples_china[["samples"]] > 60) /
  length(general_samples_china[["samples"]])*100
china_icu_over_60 <- sum(icu_samples_china[["samples"]] > 60) /
  length(icu_samples_china[["samples"]])*100
world_icu_over_60 <- sum(icu_samples_world[["samples"]] > 60) /
  length(icu_samples_world[["samples"]])*100
world_general_over_60 <- sum(general_samples_world[["samples"]] > 60) /
  length(general_samples_world[["samples"]])*100




####### COMPARE COMPLETE VS ONGOING STUDIES #######

#printed values describe error in fit. Ideally less than 0.001
general_samples_china_ongoing <- create_dist_weibull_discrete(los_general_china_ongoing_s,
                                                      sizes, 
                                                      sample_size = sample_size, 
                                                      init_values = c(3,27))

#printed values describe error in fit. Ideally less than 0.001
general_samples_china_complete <- create_dist_weibull_discrete(los_general_china_complete_s,
                                                              sizes, 
                                                              sample_size = sample_size, 
                                                              init_values = c(3,27))


HIST_PLOT_2 <- plot_hist_2(china_ongoing = general_samples_china_ongoing[["samples"]], 
                         china_complete = general_samples_china_complete[["samples"]])

pdf("histograms_complete.pdf")
HIST_PLOT_2
dev.off()

quants_china_ongoing <- quantile(general_samples_china_ongoing[["samples"]], probs=iqr)
quants_china_complete <- quantile(general_samples_china_complete[["samples"]], probs=iqr)
china_complete_over_60 <- sum(general_samples_china_complete[["samples"]] > 60) /
  length(general_samples_china_complete[["samples"]])*100
china_ongoing_over_60 <- sum(general_samples_china_ongoing[["samples"]] > 60) /
  length(general_samples_china_ongoing[["samples"]])*100





# length_needed <- length(gamma_errors_general_china)
# combined_errors <- data_frame( values = c(gamma_errors_general_china, 
#                                           weibull_errors_general_china), 
#                               type =  c(rep("gamma", length_needed), rep("weibull", length_needed)))
# 
# ###### FIT OVERALL DISTRIBUTION ####### - doesn't work, probably because of 0s
# #dweibull_overall <- estdweibull(all_samples_general)
# HIST_PLOT <- ggplot(combined_errors, aes(x=values)) + 
#   geom_histogram(bins=33)+ 
#   facet_grid(~type) + theme_bw() #+ 
#  # labs(x ="Length of Stay (days)", y="Counts") #+
# # geom_vline(aes(xintercept = z), vline_data, colour = "black", linetype= "dashed")
# sum(gamma_errors_general_china)
# sum(weibull_errors_general_china)

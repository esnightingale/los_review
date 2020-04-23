# Fits a weibull distribution to the iqr range for each input set, then samples them
# propotionaly based on the data set size. 

# Load in the functions
setwd("~/Documents/GitHub/los_review/code/")
source("comb_dist_funcs.R")
source("comb_dist_data.R")
###### INPUT ######

# interquatile range. Can change to something else but have to do equivalent in qunatiles
iqr <- c(0.25,0.5,0.75)
sample_size <- 100000
set.seed(643)

# Run the function to create the overall distribution.
# init_values for the fitting must be in right area to get a good fit.
# sample_size is for sampling th overall distribution at the end.

####### GENERAL ########

#printed values describe error in fit. Ideally less than 0.001
general_samples_china <- create_dist_weibull_discrete(los_general_china,
                                                        sizes, 
                                                        sample_size = sample_size, 
                                                        init_values = c(3,27))

# store the samples
all_samples_general_china <- general_samples_china[[1]]
# store the weibul parameters
fitted_pars_china <- general_samples_china[[2]]
#caluclate the new quanitles
quantile(all_samples_general_china, probs=c(0.25,0.5,0.75))
general_hist_china <-hist(all_samples_general_china, breaks=40)

general_samples_world <- create_dist_weibull_discrete(los_general_world,
                                                      sizes, 
                                                      sample_size = sample_size, 
                                                      init_values = c(3,27))

# store the samples
all_samples_general_world <- general_samples_world[[1]]
# store the weibul parameters
fitted_pars_world <- general_samples_world[[2]]
#caluclate the new quanitles
quantile(all_samples_general_world, probs=c(0.25,0.5,0.75))
general_hist_world <- hist(all_samples_general_world, breaks=40)

######## ICU ######

#printed values describe error in fit. Ideally less than 0.001
colnames(los_icu) <- c("N", "LOS_med", "LOS_q25", "LOS_q75", "LOS_mean", "LOS_sd")
colnames(los_icu_china) <- c("N", "LOS_med", "LOS_q25", "LOS_q75", "LOS_mean", "LOS_sd")
colnames(los_icu_world) <- c("N", "LOS_med", "LOS_q25", "LOS_q75", "LOS_mean", "LOS_sd")


icu_samples_china <- create_dist_weibull_discrete(los_icu_china,
                                                sizes, 
                                                sample_size = sample_size, 
                                                init_values = c(3,27))

# store the samples
all_samples_icu_china  <- icu_samples_china [[1]]
# store the weibul parameters
fitted_pars_china  <- icu_samples_china [[2]]
#caluclate the new quanitles
quantile(all_samples_icu_china , probs=c(0.25,0.5,0.75))
icu_hist_china <-hist(all_samples_icu_china , breaks=20)

icu_samples_world <- create_dist_weibull_discrete(los_icu_world,
                                                  sizes, 
                                                  sample_size = sample_size, 
                                                  init_values = c(3,27))

# store the samples
all_samples_icu_world  <- icu_samples_world [[1]]
# store the weibul parameters
fitted_pars_world  <- icu_samples_world [[2]]
#caluclate the new quanitles
quantile(all_samples_icu_world , probs=c(0.25,0.5,0.75))
icu_hist_world<-hist(all_samples_icu_world , breaks=20)


HIST_PLOT <- plot_hist(icu_china = all_samples_icu_china, 
                       icu_world = all_samples_icu_world, 
                       general_china = all_samples_general_china,
                       general_world = all_samples_general_world)

pdf("histograms.pdf")

HIST_PLOT

dev.off()

###### FIT OVERALL DISTRIBUTION ####### - doesn't work, probably because of 0s
#dweibull_overall <- estdweibull(all_samples_general)



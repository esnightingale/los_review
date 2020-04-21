# Fits a weibull distribution to the iqr range for each input set, then samples them
# propotionaly based on the data set size. 

# Load in the functions
source("comb_dist_funcs.R")
source("comb_dist_data.R")
###### INPUT ######

# interquatile range. Can change to something else but have to do equivalent in qunatiles
iqr <- c(0.25,0.5,0.75)

# Run the function to create the overall distribution.
# init_values for the fitting must be in right area to get a good fit.
# sample_size is for sampling th overall distribution at the end.

####### GENERAL ########

#printed values describe error in fit. Ideally less than 0.001
general_samples <- create_dist_weibull_discrete(los_general,
                                                        sizes, 
                                                        sample_size = 10000000, 
                                                        init_values = c(3,27))

# store the samples
all_samples_general <- general_samples[[1]]
# store the weibul parameters
fitted_pars <- general_samples[[2]]
#caluclate the new quanitles
quantile(all_samples_general, probs=c(0.25,0.5,0.75))
hist(all_samples_general, breaks=20)

######## ICU ######

#printed values describe error in fit. Ideally less than 0.001
colnames(los_icu) <- c("N", "LOS_med", "LOS_q25", "LOS_q75", "LOS_mean", "LOS_sd")
icu_samples <- create_dist_weibull_discrete(los_icu,
                                                sizes, 
                                                sample_size = 10000000, 
                                                init_values = c(3,27))

# store the samples
all_samples_icu <- icu_samples[[1]]
# store the weibul parameters
fitted_pars <- icu_samples[[2]]
#caluclate the new quanitles
quantile(all_samples_icu, probs=c(0.25,0.5,0.75))
hist(all_samples_icu, breaks=20)



###### FIT OVERALL DISTRIBUTION ####### - doesn't work, probably because of 0s
dweibull_overall <- estdweibull(all_samples_general)



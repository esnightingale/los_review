# Fits a weibull distribution to the iqr range for each input set, then samples them
# propotionaly based on the data set size. 

# FUNCTIONS TO USE 
# 1. create_dist_dweibull - fits to a discretised weibull. iit values under 1
# 2. create_dist_weibull_continuos - fits to weibull, final samples from weibulls
# 3. create_dist_weibull_discrete - fits to weibull, final samples from disecretised weibulls

# for the fitting check use plot_fit_dweibull for options 1, plot_fit_weibull for 2 and 3.
#test data - Chen et al. 


# NOTE: when using functions from the DiscreteWeibull package using type 1 weibull distribution
# -type 1 mimics cdf of continuos weibull
# -type3 (the other option, not used) - generalises notions of hazard rate and mean residual life.. 
# See Babiero 2013: Parameter estimation from type 3 Weibull estimation: a comparative study

# Load in the functions
source("comb_dist_funcs.R")

###### INPUT ######

# quantiles (IQR and median) for each study
quants <- list(
  study1 = c(12.5,16,21.5),
  study2 = c(9,12,14), 
  study3 = c(13,14,17)
)
# Number of participants in each study
sizes <- c(
  study1 = 9,
  study2 = 121,
  study3 = 29
)
# interquatile range. Can change to something else but have to do equivalent in qunatiles
iqr <- c(0.25,0.5,0.75)

####### CREATE SAMPLES ########

# Run the function to create the over distribution.
# init_values for the fitting must be in right area to get a good fit.
# sample_size is for sampling th overall distribution at the end.

#printed values describe error in fit. Ideally less than 0.001
sampling_output <- create_dist_weibull_discrete(quants,
                                                        sizes, 
                                                        sample_size = 1000, 
                                                        init_values = c(27,3))
# store the samples
all_samples <- sampling_output[[1]]
# store the weibul parameters
fitted_pars <- sampling_output[[2]]
#caluclate the new quanitles
quantile(all_samples, probs=c(0.25,0.5,0.75))
# for test case they should be: 10, 12, 15

# create plots to visualise the fit
out <- plot_fit_weibull(fitted_pars)
# visualise the fit. can 
do.call("grid.arrange", c(out, ncol = round(length(quants)/3)))  

###### FIT OVERALL DISTRIBUTION #######

#### CONTINOUS 
# fit an overall - only use with the continous output
weibull_overall <- fitdistr(all_samples, "weibull")
# discretising the weibull
weibull_overall_discrete <- distcrete("weibull", shape = weibull_overall[[1]]["shape"], 
                                      scale = weibull_overall[[1]]["scale"], 
                                      interval = 1)

#### DISCRETE
dweibull_overall <- estdweibull(all_samples)



# LOS Distributions to use. 

# Analysis of hospital length of stay with COVID-19
# Author: Naomi R Waterlow
# Date: 2020-05-06
################################################################################
#
# This script allows users to gerenate their own sample from a specificed distribution
# 
################################################################################
################################################################################

setwd("~/Documents/GitHub/los_review/code/")
# Load the functions
source("comb_dist_funcs.R")
#Load and format the data
source("comb_dist_data.R")

# Create the distribution
# Input: sample size
#        location - "China" or "Rest_of_world",
#        hospital = "General" or "ICU"
# Output: samples - samples taken from desired distribution
#         parameters - weibull parameters and sample size for each fitted distribution.
#         errors - the magnittude of error for each fit.

calculated_distribution <- create_own_distribution(sample_size = 100000, 
                                                location = "China",
                                                hospital = "General")

#extract the sample size
distribution_samples <- calculated_distribution[["samples"]]

hist(distribution_samples, breaks= 50)

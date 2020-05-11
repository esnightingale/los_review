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

library(here)

# Load the functions
source(here::here("code","comb_dist_funcs.R"))
#Load and format the data
source(here::here("code","comb_dist_data.R"))

# Create the distribution
# Input: sample size
#        setting - "China" or "Rest_of_world",
#        type = "General" or "ICU"
# Output: samples - samples taken from desired distribution
#         parameters - weibull parameters and sample size for each fitted distribution.
#         errors - the magnitude of error for each fit.

n <- 100000
calculated_distribution <- create_own_distribution(sample_size = n, 
                                                setting = "China",
                                                type = "General")

#extract the sample size
distribution_samples <- calculated_distribution[["samples"]]

hist(distribution_samples, breaks= 50)

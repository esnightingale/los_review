#Functions for combining_distributions.R

#devtools::install_github("reconhub/distcrete")

library(ggplot2)
library(gridExtra)
library(distcrete)
library(fitdistrplus)
library(tidyr)
library(dplyr)
library(DiscreteWeibull)
library(mixdist)

######### FIT IQR'S ########


# calculate the difference between quantiles and weibell output for theta
min_quantiles <- function(theta, dist_x, iqr = c(0.25,0.5,0.75)){
  #calculate quantiles with test parameters
  test_quantiles <- pweibull(q = dist_x, shape = theta[1], scale = theta[2])
  actual_quantiles <- iqr
  # calculate difference between actualy quantiles and tested quantiles
  diff_quantiles <- test_quantiles - actual_quantiles
  # make it absolute
  optim_value <- sum(diff_quantiles^2)
  return(optim_value)
}

# optimising wrapper over the min_quantiles function - weibull
optimise_quantiles <- function(init_values, dist_x, iqr = c(0.25,0.5,0.75)){
  #optimise the min_quantiles function. 
  #suppressWarnings so don't get message about trying values that don't work
  par_out <- suppressWarnings(optim(par = c(init_values[1], init_values[2]),
        fn = min_quantiles,
        dist_x = c(dist_x[c("LOS_q25", "LOS_med", "LOS_q75")]),
        iqr=iqr))
  
  par_out <- c(par_out, dist_x["N"])
  
  return(par_out)
}


weibull_mean <- function(input){
 # calculate the shape and scale parameters from the mean and sd
  parameters <- weibullpar(mu = input["LOS_mean"], sigma = input["LOS_sd"])
  parameters <- c(shape = parameters$shape, scale = parameters$scale, 
                   N = input[["N"]])
  return(parameters)
}

######### OVERALL WRAPPERS ######

#calculate the overall sample
create_dist_weibull_discrete <- function(quants, sizes, sample_size=10000, init_values){
  # for subset that contains medians
  quants_iqr <- quants[which(!is.na(quants$LOS_med)),]
  quants_mean <- quants[which(is.na(quants$LOS_med)),]
  # optimise the fit to dweibull for each input set
  weibull_all <- apply(quants_iqr,1, function(x) optimise_quantiles(init_values = init_values, 
                                                                 dist_x = x))
  # save the parameters and errors
  weibull_pars <-   lapply(weibull_all, function(x) format_pars(x))
  weibull_pars <- data.frame(matrix(unlist(weibull_pars), ncol=length(weibull_pars), byrow=F))
  weibull_errors <- lapply(weibull_all, function(x) x[[2]])
  # print the errors to alert the user if the errors are very big
  print(weibull_errors)
  
  # calculate the weibull paraamters from the mean and sds 
  weibull_means <- apply(quants_mean,1, function(x) weibull_mean(x))
  #TODO add to the weibull_all list
  all_dists <- cbind(weibull_means, weibull_pars)
  #create discrete functions for each distribution
  dis_weibulls <- lapply(all_dists, function(x) discrete_dist(x)) 
  # calculate the propotional sample sizes
  all_dists["prop_samples",] <- sapply(all_dists["N",], function(x) x/sum(all_dists["N",]))
  
  # sample from multinomial to determine how many targets to include
  all_dists["samples_taken",] <- rmultinom(n = 1, size = 1000, prob = all_dists["prop_samples",])
  # sample from the overall distributions
  all_samples <- c()
  for(i in 1:length(dis_weibulls)){
    # get samples from the discrete weibull for each input set
    subset_samples <- dis_weibulls[[i]]$r(n = all_dists["samples_taken",i])
    all_samples <- c(all_samples, subset_samples)
  }
  return(list(all_samples, weibull_pars))
}


########## OTHER FUNCTIONS ########

# discretise the output
discrete_dist <- function(weibull_pars){

  dist_weibull <- distcrete(name = "weibull",
                            interval = 1, 
                            w = 0.05,
                            shape = weibull_pars[1], 
                            scale = weibull_pars[2]) 

}

# format the parameters
format_pars <- function(input_list){
  pars <- c(shape = input_list[[1]][1], scale = input_list[[1]][2],
            N = input_list[["N"]])
  return(pars)
}


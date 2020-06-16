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
library(data.table)

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

# calculate the difference between quantiles and gamma output for theta
min_quantiles_gamma <- function(theta, dist_x, iqr = c(0.25,0.5,0.75)){
  #calculate quantiles with test parameters
  test_quantiles <- pgamma(q = dist_x, shape = theta[1], scale = theta[2])
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

# optimising wrapper over the min_quantiles function - weibull
optimise_quantiles_gamma <- function(init_values, dist_x, iqr = c(0.25,0.5,0.75)){
  #optimise the min_quantiles function. 
  #suppressWarnings so don't get message about trying values that don't work
  par_out <- suppressWarnings(optim(par = c(init_values[1], init_values[2]),
                                    fn = min_quantiles_gamma,
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

######### OVERALL WRAPPER ######

errors_gamma <-function(quants, sizes, sample_size=10000, init_values){
  # for subset that contains medians
  quants_iqr <- quants[which(!is.na(quants$LOS_med)),]
  quants_mean <- quants[which(is.na(quants$LOS_med)),]
  
  # optimise the fit to dweibull for each input set
  gamma_all <- apply(quants_iqr,1, function(x) optimise_quantiles_gamma(init_values = init_values, 
                                                                    dist_x = x))
  # save the parameters and errors
  gamma_errors <- lapply(gamma_all, function(x) x[[2]])
  gamma_errors <- unlist(gamma_errors)
  return(gamma_errors)
}


#calculate the overall sample
create_dist_weibull_discrete <- function(quants, sample_size=10000, init_values,
                                         weighting = TRUE){
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
  weibull_errors <- unlist(weibull_errors)
  # print the errors to alert the user if the errors are very big
  
  if(dim(quants_mean)[1] >0){
  # calculate the weibull paraamters from the mean and sds 
  weibull_means <- apply(quants_mean,1, function(x) weibull_mean(x))
  #TODO add to the weibull_all list
  all_dists <- cbind(weibull_means, weibull_pars) } else {
    all_dists <- weibull_pars
    rownames(all_dists) <- c("shape", "scale", "N")
  }

  #create discrete functions for each distribution
  dis_weibulls <- lapply(all_dists, function(x) discrete_dist(x)) 
  # calculate the propotional sample sizes
  if (weighting == T){
  all_dists["prop_samples",] <- sapply(all_dists["N",], function(x) x/sum(all_dists["N",]))
  } else{ 
    all_dists["prop_samples",] <- sapply(all_dists["N",], function(x) 1/dim(all_dists)[2])}
  # sample from multinomial to determine how many targets to include
  all_dists["samples_taken",] <- rmultinom(n = 1, size = sample_size, prob = all_dists["prop_samples",])
  # sample from the overall distributions
  all_samples <- c()
  for(i in 1:length(dis_weibulls)){
    # get samples from the discrete weibull for each input set
    subset_samples <- dis_weibulls[[i]]$r(n = all_dists["samples_taken",i])
    all_samples <- c(all_samples, subset_samples)
  }
  return(list(samples = all_samples, parameters = weibull_pars, errors = weibull_errors))
}


########## OTHER FUNCTIONS ########

# discretise the output
discrete_dist <- function(weibull_pars){

  dist_weibull <- distcrete(name = "weibull",
                            interval = 1, 
                            w = 0.5,
                            shape = weibull_pars[1], 
                            scale = weibull_pars[2]) 

}

# format the parameters
format_pars <- function(input_list){
  pars <- c(shape = input_list[[1]][1], scale = input_list[[1]][2],
            N = input_list[["N"]])
  return(pars)
}



####### PLOTS ######ß

plot_hist_1 <- function(icu_china, icu_world, general_china, general_world){

  icu_china <- data.frame(samples =icu_china, location = "China", type = "ICU")
  icu_world <- data.frame(samples =icu_world, location = "Rest of World", type = "ICU")
  general_china <- data.frame(samples =general_china, location = "China", type = "Total")
  general_world <- data.frame(samples =general_world, location = "Rest of World", type = "Total")
  
  all_samples <- rbind(icu_china, icu_world, general_china, general_world)

 vline_data <- all_samples %>%
    group_by(location,type) %>%
  summarize(z=median(samples))

  HIST_PLOT <- ggplot(all_samples, aes(x=samples)) + 
    geom_histogram(bins=61)+ 
    facet_grid(location~type) + theme_bw() + 
    scale_x_continuous(breaks = seq(0, 60, by = 5), limits=c(0,60)) + 
    labs(x ="Length of Stay (days)", y="Counts") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) 
  return(HIST_PLOT)
  
}

plot_hist_2 <- function(china_ongoing, china_complete){
  
  china_ongoing <- data.frame(samples =china_ongoing, type = "Ongoing")
  china_complete <- data.frame(samples =china_complete, type = "Complete")

  
  all_samples <- rbind(china_ongoing, china_complete)
  
  
  HIST_PLOT <- ggplot(all_samples, aes(x=samples)) + 
    geom_histogram(bins=61)+ 
    facet_grid(~type) + theme_bw() + 
    scale_x_continuous(breaks = seq(0, 60, by = 5), limits=c(0,60)) + 
    labs(x ="Length of Stay (days)", y="Counts") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) 
  
  return(HIST_PLOT)
  
}


#function to create custom distributions
create_own_distribution <- function(sample_size = 10000, setting, type, init_values = c(3,27)){

  create_renamer <- function(x){
    rename_all(x, .funs = function(v){sub(pattern="_ICU", replacement="", v)})
  }
  
  if ( setting == "China" && type == "General"){
    # run the function to create an overall distribution for los in China, General LOS
    samples_generated <- create_dist_weibull_discrete(los_general_china_s,
                                                          sample_size = sample_size, 
                                                          init_values = init_values)
  }
  
  if ( setting == "Rest_of_World" & type == "General"){
    # run the function to create an overall distribution for los in China, General LOS
    samples_generated <- create_dist_weibull_discrete(los_general_world_s,
                                                          sample_size = sample_size, 
                                                          init_values = init_values)
  }
  
  if ( setting == "Rest_of_World" & type == "ICU"){
    # run the function to create an overall distribution for los in China, ICU LOS
    
    samples_generated <- create_dist_weibull_discrete(create_renamer(los_icu_world_s),
                                                          sample_size = sample_size, 
                                                          init_values = init_values)
  }
  
  if ( setting == "China" & type == "ICU"){
    # run the function to create an overall distribution for los in China, ICU LOS
    samples_generated <- create_dist_weibull_discrete(create_renamer(los_icu_china_s),
                                                          sample_size = sample_size, 
                                                          init_values = init_values)
  }
  
  return(samples_generated)
}


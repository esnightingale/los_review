#Functions for combining_distributions.R

devtools::install_github("reconhub/distcrete")

library(ggplot2)
library(gridExtra)
library(distcrete)
library(DiscreteWeibull)
library(fitdistrplus)

######### FIT IQR'S ########


# calculate the difference between quantiles and discretised weibell output for theta
min_quantiles <- function(theta, dist_x, iqr = c(0.25,0.5,0.75)){
  #calculate quantiles with test parameters
  test_quantiles <- pdweibull(x = dist_x, q = theta[1], beta = theta[2])
  actual_quantiles <- iqr
  # calculate difference between actualy quantiles and tested quantiles
  diff_quantiles <- test_quantiles - actual_quantiles
  # make it absolute
  optim_value <- sum(diff_quantiles^2)
  return(optim_value)
}
# calculate the difference between quantiles and weibell output for theta
min_quantiles_2 <- function(theta, dist_x, iqr = c(0.25,0.5,0.75)){
  #calculate quantiles with test parameters
  test_quantiles <- pweibull(q = dist_x, scale = theta[1], shape = theta[2])
  actual_quantiles <- iqr
  # calculate difference between actualy quantiles and tested quantiles
  diff_quantiles <- test_quantiles - actual_quantiles
  # make it absolute
  optim_value <- sum(diff_quantiles^2)
  return(optim_value)
}


# optimising wrapper over the min_quantiles function = discreteised weibull
optimise_quantiles <- function(init_values, dist_x, iqr = c(0.25,0.5,0.75)){
  #optimise the min_quantiles function. 
  #suppressWarnings so don't get message about trying values that don't work
 suppressWarnings( optim(par = c(init_values[1], init_values[2]),
        fn = min_quantiles,
        dist_x = dist_x,
        iqr=iqr))
}

# optimising wrapper over the min_quantiles function - weibull
optimise_quantiles_2 <- function(init_values, dist_x, iqr = c(0.25,0.5,0.75)){
  #optimise the min_quantiles function. 
  #suppressWarnings so don't get message about trying values that don't work
  suppressWarnings(optim(par = c(init_values[1], init_values[2]),
        fn = min_quantiles_2,
        dist_x = dist_x,
        iqr=iqr))
}


######### OVERALL WRAPPERS ######

#calculate the overall sample
create_dist_dweibull <- function(quants, sizes, sample_size=10000, init_values=c(0.5,0.5)){
  # optimise the fit to dweibull for each input set
  dweibull_all <- lapply(quants, function(x) optimise_quantiles(init_values = init_values, 
                                                                dist_x = x))
  # save the parameters and errors
  dweibull_pars <-   lapply(dweibull_all, function(x) x[[1]])
  dweibull_errors <- lapply(dweibull_all, function(x) x[[2]])
  # pring the errors to alert the user if the errors are very big
  print(dweibull_errors)
  
  # calculate the propotional sample sizes
  prop_samples <- sapply(sizes, function(x) x/sum(sizes))
  
  all_samples <- c()
  for(i in 1:length(dweibull)){
    # get samples from the weibull for each input set
    subset_samples <- rdweibull(n = prop_samples[[i]]*sample_size, 
                                q = dweibull_pars[[i]][1], 
                                beta = dweibull_pars[[i]][2])
    all_samples <- c(all_samples, subset_samples)
    
  }
  #print a histogram
  hist(all_samples)
  return(list(all_samples, dweibull_pars))
}

#calculate the overall sample
create_dist_weibull_continous <- function(quants, sizes, sample_size=10000, init_values){
  # optimise the fit to dweibull for each input set
  weibull_all <- lapply(quants, function(x) optimise_quantiles_2(init_values = init_values, 
                                                                 dist_x = x))
  # save the parameters and errors
  weibull_pars <-   lapply(weibull_all, function(x) x[[1]])
  weibull_errors <- lapply(weibull_all, function(x) x[[2]])
  # print the errors to alert the user if the errors are very big
  print(weibull_errors)
  
  # calculate the propotional sample sizes
  prop_samples <- sapply(sizes, function(x) x/sum(sizes))
  
  all_samples <- c()
  for(i in 1:length(weibull_all)){
    # get samples from the weibull for each input set
    subset_samples <- rweibull(n = prop_samples[[i]]*sample_size, 
                               scale = weibull_pars[[i]][1], 
                               shape = weibull_pars[[i]][2])
    all_samples <- c(all_samples, subset_samples)
    
  }
  # print a histogram
  hist(all_samples)
  return(list(all_samples, weibull_pars))
}

#calculate the overall sample
create_dist_weibull_discrete <- function(quants, sizes, sample_size=10000, init_values){
  # optimise the fit to dweibull for each input set
  weibull_all <- lapply(quants, function(x) optimise_quantiles_2(init_values = init_values, 
                                                                 dist_x = x))
  # save the parameters and errors
  weibull_pars <-   lapply(weibull_all, function(x) x[[1]])
  weibull_errors <- lapply(weibull_all, function(x) x[[2]])
  # print the errors to alert the user if the errors are very big
  print(weibull_errors)
  
  #create discrete functions for each distribution
  dis_weibulls <- lapply(weibull_pars, function(x) discrete_dist(x)) 
  
  # calculate the propotional sample sizes
  prop_samples <- sapply(sizes, function(x) x/sum(sizes))
  
  all_samples <- c()
  for(i in 1:length(weibull_all)){
    # get samples from the discrete weibull for each input set
    subset_samples <- dis_weibulls[[i]]$r(n = prop_samples[[i]]*sample_size)
    all_samples <- c(all_samples, subset_samples)
  }
  # print a histogram
  hist(all_samples)
  return(list(all_samples, weibull_pars))
}

####### CREATE PLOTS ########

# create plots of individual fits to check quality
plot_fit_dweibull <- function(fitted_pars){
  visulations <- list()
  x_axis <- c(1:40)
  for(i in 1:length(fitted_pars)){
    # calculate the curve of the pdf
    y_axis <- pdweibull(x=x_axis, q=fitted_pars[[i]][1], beta=fitted_pars[[i]][2])
    dat_plot <- data.frame(x_axis, y_axis)
    dat_points <- data.frame(quants[[i]], iqr)
    colnames(dat_points) <- c("quants", "iqr")
    dat_points$quant_scaled <- c()
    #plot each graph
    visulations[[i]] <- ggplot(dat_plot, aes(x=x_axis, y=y_axis))+
      geom_line() + geom_point(data =dat_points, aes(x=quants, y = iqr))
  }
  return(visulations)
}

# create plots of individual fits to check quality
plot_fit_weibull <- function(fitted_pars){
  visulations <- list()
  x_axis <- c(1:40)
  for(i in 1:length(fitted_pars)){
    # calcualte the point on pdf curve
    y_axis <- pweibull(q=x_axis, scale=fitted_pars[[i]][1], shape=fitted_pars[[i]][2])
    dat_plot <- data.frame(x_axis, y_axis)
    dat_points <- data.frame(quants[[i]], iqr)
    colnames(dat_points) <- c("quants", "iqr")
    dat_points$quant_scaled <- c()
    #plot each graph
    visulations[[i]] <- ggplot(dat_plot, aes(x=x_axis, y=y_axis))+
      geom_line() + geom_point(data =dat_points, aes(x=quants, y = iqr))
  }
  return(visulations)
}


########## OTHER FUNCTIONS ########

# discretise the output
discrete_dist <- function(weibull_pars){
  dist_weibull <- distcrete(name = "weibull",
                            interval = 1, 
                            shape = weibull_pars[2], 
                            scale = weibull_pars[1]) 

}



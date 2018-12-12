#### Script Information ----
#### Functions for use validating data
#### Author: MS Patterson
#### email: tertiarymatt@gmail.com
#### last updated: 12/12/18

#### Required packages ----



#### This section is for functions related to sample size determination ----

genSample1 <- function(p0 = 0.85, h = 0.01, alpha = 0.95){
  # convert the input probalities into z scores
  z_alpha <- qnorm(alpha + 0.025)
  # calculate n_prime sample size
  n_prime <- z_alpha^2 * (p0 * (1 - p0)) / (h^2)
  
  return(round(n_prime))
}

genSample2 <- function(p0 = 0.85, min_diff = 0.01, alpha = 0.95, beta = 0.95){
  # convert the input probalities into z scores
  z_alpha <- qnorm(alpha + 0.025)
  z_beta <-  qnorm(beta + 0.025)
  
  # calculate the actual n prime estimate 
  p1 <- p0 - min_diff
  noom <- (z_alpha * sqrt(p0 * (1 - p0))) + (z_beta * sqrt(p1 * (1 - p1)))
  n_prime <- (noom / min_diff)^2
  
  return(round(n_prime))
}

contCorrect <- function(n_prime, min_diff = 0.01, p0 = 0.85){
  n <- (n_prime / 4) * (1 + sqrt(1 + 2 / (n_prime * min_diff)))^2
  
  return(round(n))
}

powerEst <- function(n, min_diff, p0, alpha=0.95){
  p1 <- p0 - min_diff
  z_alpha <- qnorm(alpha + 0.025)
  noom <- sqrt(n) * min_diff - (1 / (2 * sqrt(n))) - z_alpha * sqrt(p0 * min_diff)
  denoom <- sqrt(p1 * (1 - p1))
  pow <- pnorm(noom/denoom)
  return(pow)
}

#### Optimizing sample split ---- This section relies on work in Wagner J.E.
#### & S. V. Stehman. 2015. Optimizing sample size allocation to strata for
#### estimating area and map accuracy. Remote Sens. Environ. 168: 126???133.

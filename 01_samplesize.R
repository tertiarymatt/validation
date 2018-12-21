#' ---
#' title: "Determing Sample Size"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' ### Required packages
#+ Packages
library(tidyverse)
source("00_functions.R")

#' ### This section is used to determing sample size needed for the project.
#' 
#'+ Sample Size

overallN <- genSample1(p0 = 0.75, h = 0.01, alpha = 0.05)

deforestationN <- genSample1(p0 = 0.90, h = 0.01, alpha = 0.05)

averageN <- mean(c(overallN, deforestationN))

#' N needs to be increased a bit to cover the possibility of bad imagery, etc. 

#'+ Set final sample size
actualN <- 5500

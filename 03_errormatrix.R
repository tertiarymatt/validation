#' ---
#' title: "Error Matrix Generation"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' ### Required packages
#+ Packages
library(tidyverse)
library(irr)
library(e1701)
source("00_functions.R")

#' Task list for this script:
#' 1. Bring in validation data. Need to have map values attached.
#' 1. Convert map values and validation values to factors.
#' 1. Ensure factors have identical levels.
#' 1. Construct confusion matrix.
#' 1. Convert to area-based. 
#' 1. Calculate standard errors of area estimates. 
#' 1. Calculate error metrics.
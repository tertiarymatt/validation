#' ---
#' title: "Functions"
#' author: "MS Patterson, tertiarymatt@gmail.com; Paolo Arevalo"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' This script contains the functions required for the calculation of the
#' unbiased areas of map classes when the samples were produced using the
#' strata of a different map:  

#' 1. Function to calculate strata for any given pair of years  
#' 1. Calculate unbiased area proportions and variance of reference classes  
#' 1. Calculate standard error of unbiased area proportion  
#' 1. Calculate unbiased areas, their confidence interval and margin of error  
#' The functions in this script are based on Stehman S. V. 2014. 
#' *Estimating area and map accuracy for stratified random sampling when the
#'  strata are different from the map classes.* 
#'  Int. J. Remote Sens. 35: 4923-4939.  

#+ ProjectSetup
require(tidyverse)
source("00.3_area_est_functions_en.R")
#setwd("R/projects")

# Input data from Stehman, 2014
stehman <- read_csv(file = "data/test/stehman_test_data.csv")

#' A little test with data from Stehman, 2014.  
#+ TestBlock1
orig_strata <- stehman$stratum
ref_label <- stehman$ref_class
map_label <- stehman$map_class
strata_totals <- data.frame(c(1, 2, 3, 4), c(40000, 30000, 20000, 10000))
sample_totals <- data.frame(c(1, 2, 3, 4), c(10, 10, 10, 10))
rfcodes <- c('A', 'B', 'C', 'D')
propsAndVars <- calcPropsAndVars(orig_strata, ref_label, map_label, 
										strata_totals, sample_totals, rfcodes)


#' A little test with data from Stehman, 2014.  
#+ TestBlock2
totarea_pix <- sum(strata_totals)
ref_var <- propsAndVars$ref_var

propSE <- calcPropSE(strata_totals, sample_totals, ref_var, 
											 rfcodes, totarea_pix)

#' A little test with data from Stehman, 2014.  
#+ TestBlock3
class_prop <- propsAndVars$class_prop
pixel <- 30

unArea <- calcUnbiasedArea(totarea_pix, class_prop, propSE, pixel)

#' A little test with data from Stehman, 2014.  
#+ TestBlock4
accurates <- calcAccuracies(strata_totals, sample_totals, rfcodes, totarea_pix,
									propsAndVars)
accurates

#' ### Summary of implementing this approach

#' To run this on other data, here are the data one needs:  
#' 1. **orig_strata**, a vector with numeric codes representing the original 
#' stratification of each sample.  
#' 2. **ref_label**, a vector with numeric codes representing the reference label 
#' for that year/map, for each sample.  
#' 3. **map_label**, a vector with numeric codes representing the map labels, for
#' each sample.  
#' 4. **strata_totals**, a dataframe with two columns and number of rows equal to 
#' the total number of classes in the original strata. The first column must 
#' have the same codes found in the original stratification and the second must 
#' have the total number of PIXELS of each class in that original strata map.  
#' 5. **sample_totals**, a dataframe with two columns and number of rows equal to 
#' the total number of classes in the original strata. The first column must 
#' have the same codes found in the original stratification, and the second must
#' have the total number of SAMPLE UNITS of each class collected from that 
#' original strata map.  
#' 6. **rfcodes**, a vector with numeric values representing the reference codes
#' present in ALL of the periods.  
#' 7. **pixel**, the pixel size of the maps being analyzed.  
#' 8. **totarea_pix**, an integer with the total number of pixels present in the 
#' original stratification map (this can be calculated from other values).  
#' 
#' ####TODO  
#' 1. [ ] **orig_strata**   
#' 2. [ ] **ref_label**  
#' 3. [ ] **map_label**  
#' 4. [ ] **strata_totals**  
#' 5. [ ] **sample_totals**    
#' 6. [ ] **rfcodes**  
#' 7. [ ] **pixel**  
#' 8. [ ] **totarea_pix**  
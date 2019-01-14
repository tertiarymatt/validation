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
library(caret)
source("00_functions.R")

#' Task list for this script:  
#' 1. DONE. Bring in validation data. Need to have map values attached.  
#' 1. DONE. Convert map values and validation values to factors.  
#' 1. DONE. Ensure factors have identical levels.  
#' 1. DONE. Construct confusion matrix.  
#' 1. Convert to area-based error matrix.   
#' 1. Calculate standard errors of area estimates. 
#' 1. Calculate error metrics.  
#' 

#' ### Data Prep  
#' Import the validation data. Either properly configure and source
#'  02_dataprep.R or run it manually.
 
#+ Import
# Import the collected validaiton data and map values. 
source("02_dataprep.R")

#' The error matrix tool works with factors. The data have been imported and
#' prepared as characters. Convert map values and validation values to factors.

#+ ConvertoClassesandFactors
#Code to convert these classes into factors, for use in building an error matrix.

# convert integers into text
finalTable <- convertToClasses(reclassed)

#strip out No_Data entries.
toRemove <- which(finalTable$LEVEL2 == "No_Data")
finalTable <- finalTable[-toRemove,]

# Convert to factors.

refLevels <- c("Primary_Forest", "Secondary_Forest", "Plantation_Forest", 
							 "Mangrove", "Cropland", "Natural_Water",  "Artificial_Water",
							 "Paramo", "Shrubland", "Herbland", "Settlement", "Infrastructure", 
							 "Glacier", "Non-vegetated")

reference <- factor(finalTable$LEVEL2, refLevels)
map <- factor(finalTable$MapClass, refLevels)

#' ###A Basic Confusion Matrix 
#' Construct the confusion matrix, using the extracted vectors of values.
#' We can use the `caret` package for this process. 

#+ ConfusionMatrix
erroMatrix <- confusionMatrix(map, reference, positive = NULL, 
															dnn = c("Prediction", "Reference"))

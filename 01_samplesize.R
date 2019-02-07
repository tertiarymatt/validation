#' ---
#' title: "Determining Sample Size"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' Set working directory to where data is being stored.
#'+ setwd
setwd("~/R/projects/validation")

#' ### Required packages
#' You may need to install tidyverse to make this script function. To do so,
#'  uncomment the following line.
#'  install.packages("tidyverse")
#+ Packages
library(tidyverse)
source("00.2_functions_es.R")

#' ### This section is used to determing sample size needed for the project.  
#' Before running this script, use the [`01_ChangePercentage` Earth Engine 
#' script](https://code.earthengine.google.com/146c6bba7eb1d1225caa9ff01fbb3d82)
#' to find the percent area occupied by change classes in the output data. The
#' percentage of change will be displayed in the console after the script is
#' run. That percentage will be used in the sample size allocation optimizing
#' algorithm included in this script. 
#' 
#' ### Sample Size Determination  
#' After you know the percentage of change, determine the total sample size, 
#' using the following code (or one of the other sample size approaches availabe
#' in the 00_function.R).  
#' 
#' `genSample1()` is an implementation of the typical sample size
#' calcuation, using only the target accuracy (p0), the half-width of the
#' Confidence Interval (h), and the tolerance for Type I error (alpha).  

#+ Sample Size
# Calculate overall sample size
overallN <- genSample1(p0 = 0.75, h = 0.01, alpha = 0.05)
overallN

# Calculate deforestation detection sample size. 
deforestationN <- genSample1(p0 = 0.90, h = 0.01, alpha = 0.05)
deforestationN

# Find the average N, a compromise between the two. 
averageN <- mean(c(overallN, deforestationN))
averageN

#' N needs to be increased a bit to cover the possibility of bad imagery, etc. 

#+ Set final sample size
actualN <- averageN * 0.3 + averageN
actualN

#' This gives us our final sample size. Because the imagery is very 
#' patchy, we increased the sample size signficantly, as many will simply not
#' be collected. 
#' 
#' Now, we can use the optimization tool from _Wagner & Stehman, 2015_
#' to split samples into two pools: stable and change.  
#' 
#' The first step is to create an area-based error matrix, with estimated 
#' proportions. Position [1,1] in this matrix is the estimate of change area 
#' (derived from the GEE script), position [2,2] in the matrix is 
#' stable class area. Positions [1,2] and [2,1] represent the errors between 
#' the change and stable classes. These are obviously unknown, but can be 
#' estimated. Here they are conservatively estimated as being one quarter of 
#' the size of the change class. 

#+ Create Error Matrix. 
error <- matrix(nrow = 2, ncol = 2, 
                c(0.02, 0.005, 
                  0.005, 0.97))

# Check that error matrix is correctly made. 
checkErrorMatrix(error)

# Optimize the split of the sample pool. 

samplePool <- optimizeSplit(error, actualN, c("Change", "Stable"))
samplePool

#' The next step requires the use of the Google Earth Engine script 
#' [`02_SamplePointGenerator`](https://code.earthengine.google.com/1956011f22c68fe97802f62e4819de76) 
#' to assign the split samples to their respective classes, and generating a 
#' point file. R could be used for this, but this would require that the map 
#' be downloaded and imported. This script will also export a table of class 
#' areas that will be processed in the next R script. 
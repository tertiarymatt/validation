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
#' Before running this script, use the `01_ChangePercentage` Earth Engine script
#' to find the percent area occupied by change classes in the output data. That 
#' percentage will be used in the sample size allocation optimizing algorithm
#' included in this script. 
#' 
#+ Sample Size

overallN <- genSample1(p0 = 0.75, h = 0.01, alpha = 0.05)
overallN

deforestationN <- genSample1(p0 = 0.90, h = 0.01, alpha = 0.05)
deforestationN

averageN <- mean(c(overallN, deforestationN))
averageN

#' N needs to be increased a bit to cover the possibility of bad imagery, etc. 

#+ Set final sample size
actualN <- 5500

#' Use the optimization tool from Wagner & Stehman, 2015 to split samples into
#' two pools, stable and change.

#' The first step is to create an area-based error matrix, with estimated 
#' proportions. Position [1,1] in this matrix is the estimate of change area,
#' position [2,2] in the matrix is stable class area. Positions [1,2] and [2,1]
#' represent the errors between the change and stable classes. These are 
#' obviously unknown, but can be estimated. Here they are conservatively 
#' estimated as being one quarter of the size of the change class. 

#+ Create Error Matrix. 

#'We will assume a larger than realistic change to ensure sufficient power. 
error <- matrix(nrow = 2, ncol = 2, 
                c(0.025, 0.00625, 
                  0.00625, 0.9375))

# Check that error matrix is correctly made. 
checkErrorMatrix(error)

#' Next we will optimize the split of the sample pool. 

samplePool <- optimizeSplit(error, actualN, c("Change", "Stable"))
samplePool

#' The next step requires the use of the Google Earth Engine script 
#' `02_SamplePointGenerator` to assign the split samples to their respective 
#' classes, and generating a point file. R could be used for this, but this 
#' would require that the map be downloaded and imported. 
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
overallN

deforestationN <- genSample1(p0 = 0.90, h = 0.01, alpha = 0.05)
deforestationN

averageN <- mean(c(overallN, deforestationN))
averageN

#' N needs to be increased a bit to cover the possibility of bad imagery, etc. 

#'+ Set final sample size
actualN <- 5500

#' Use the optimization tool from Wagner & Stehman, 2015 to split samples into
#' two pools, stable and change.

#' The first step is to create an area-based error matrix, with estimated proportions. 

#+ Create Error Matrix.

#'We will assume a larger than realistic change to ensure sufficient power. 
error <- matrix(nrow = 2, ncol = 2, 
                c(0.05, 0.0125, 
                  0.0125, 0.925))

# Check that error matrix is correctly made. 
checkErrorMatrix(error)

#' Next we will optimize the split of the sample pool. 

samplePool <- optimizeSplit(error, actualN, c("Change", "Stable"))
samplePool

#' The next step requires the use of Google Earth Engine to assign the split
#' samples to their respective classes, and generating a point file. R could be
#' used for this, but this would require that the map be downloaded and imported. 
#' The Earth Engine script is included below. 
#' 
#'  
#' 
#' > // Stack class map and latitude/longitude  
#' > var sampleImage = ee.Image.cat(toSample, ee.Image.pixelLonLat());  
#' >   
#' > // Sample within the study area and the loss mask  
#' > var sample = sampleImage.stratifiedSample({  
#' >   numPoints: 50,  
#' >   classBand: 'class',  
#' >   region: studyArea,  
#' >   scale: 30,  
#' >   seed: 17,  
#' > });  
#' >   
#' >   // Add back in geometry and visualize  
#' > var sample_geo_assembly = sample.map(function(point){  
#' >   var long = point.get('longitude');  
#' >   var lat = point.get('latitude');  
#' >   var geom = ee.Algorithms.GeometryConstructors.Point([long, lat]);  
#' >   return point.setGeometry(geom);  
#' > });  
#' >   
#' > Map.addLayer(sample_geo_assembly.draw('0000FF',2),{},'Sample locations');  
#' > Map.centerObject(sample_geo_assembly, 9);  
#' >   
#' > print('Point Sample Data', sample_geo_assembly);  
#' >   
#' > Export.table.toDrive({  
#' >   collection: sample_geo_assembly,  
#' >   description:'Validation_Sample',  
#' >   folder:'driveFolderHere',  
#' >   fileFormat: "CSV"  
#' >   });  
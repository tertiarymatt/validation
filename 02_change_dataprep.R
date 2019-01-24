#' ---
#' title: "Change Data Prep and Exploration"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' Set working directory to where data is being stored.  
#'+ setwd
setwd("~/R/projects/validation")

#' Required packages
#+ Packages
library(tidyverse)
library(stringr)
source('00_functions.R')

#' This script is used to import the photo-interpreted points for change
#' data (two years of data) after they have been produced in Collect Earth 
#' Online. The process is as follows.  
#' 1. Import all tables.  
#' 1. Clean and shorten class names.  
#' 1. Find dominant point classes.  
#' 1. Convert dominant point classes to LULC classes. 
#' 1. Format for use in SEPAL.  
#' 1. Export the data. 
#' 1. Export class areas for use with the SAE-Analysis tool.  
#' 
#' ### Importing data.  
#' Import raw data, strip out unneeded name components of class fields. 

#+ Inmport and Prepare Data
ceoTable <- read_csv("data/ceo-two-time-training-1.3-plot-data-2019-01-16.csv")
colnames(ceoTable)

# Split table into pieces, reassemble into single year tables

#time one plot data
time1 <- ceoTable[,16:34]

#time two plot data
time2 <- ceoTable[,35:53]

#create metadata data table. Use the colnames to find them and adjust columns.
metadata <- ceoTable[,1:15]

# convert integers into text, note that map class field must be named CLASS. 
metadata <- convertToClasses(metadata)

#add metadata to the time tables.
time1 <- bind_cols(metadata, time1)
time2 <-  bind_cols(metadata, time2)

# class names need to be pulled and cleaned up.
colnames(time1)

# create class column object to use in script.
# If the structure of the data changes this MUST be updated.
classCol <- c(17:35)

classes <- colnames(time1[classCol]) %>% 
	str_split(., coll(":"), simplify = TRUE) %>% 
	.[,2] %>% 
	gsub(" ", "_", .) %>% 
	gsub("/", "_", .)

colnames(time1)[classCol] <- classes
colnames(time2)[classCol] <- classes

#verify the tables have the same names
colnames(time1) == colnames(time2)

#' ### CEO Point Table Reclassification
#'
#' Use `addTopClasses()`` to take a raw plot table produced by Collect Earth
#' Online, and returns that table with a Primary and Secondary class field
#' added.

#+ Find dominant landcover elements
time1 <- addTopClasses(time1, plotfield = 1, flagfield = 6, 
													classfields = classCol)

time2 <- addTopClasses(time2, plotfield = 1, flagfield = 6, 
											 classfields = classCol)

#' Then use primary and/or secondary classes and threshold values to convert to
#' end classification.
#' 
#' ### Level 1 and Level 2 LULC classes
#' Next steps: add code to convert to level 1 and level 2 classes, and test 
#' agreement/consistency at that level. Because the data is collected a finer
#' level of detail than either Level 1 or Level 2, Level 2 is produced first.  
#' 
#' #### Level 2 LULC Thresholds:   
#' Primary Forest = Primary tree >= 30%;  
#' Secondary Forest = Secondary tree >= 30%;  
#' Plantation = Plantation tree >= 30%;  
#' Mangrove = Mangrove >= 30%;  
#' Grass/herbland = Herbaceous veg > 0% & Tree < 30% & Shrub < 30%;  
#' Shrubland = Shrub vegetation >= 30%, Tree < 30%;  
#' Paramo = Paramo > 0%;  
#' Cropland = Crops >= 50%;  
#' Water = Natural water >= 50% | Wetland vegetation >= 50%;  
#' Settlement = Houses & Commercial >= 30% | Urban vegetation >= 30%;  
#' Infrastructure = Roads and Lots >= 30% | Infrastructure building >= 30%;  
#' Non-vegetated = barren >= 70%;  
#' Glacial = Snow/Ice >= 70%;  
#'
#' #### Point class list (ES): 
#' Arbol nativo, Arbol de rebrote, Arbol de plantacion, Arbol de Mangle
#' Vegetacion herbecea/pastos, Vegetacion arbustiva, Vegetacion de paramo
#' Cultivos Agua natural, Agua Artificial, Vegetacion de humedales Estructura de
#' vivienda, Construccion de infraestructura, Carreteras y lotes, Vegetacion de
#' asentamientos 
#' Suelo desnudo, Nieve/Hielo
#'
#' #### Point class list (EN): 
#' Forest Land: Primary tree, Secondary tree, Plantation tree, Mangrove;  
#' Grassland: Herbaceous vegetation/Grass, Shrub vegetation, Paramo vegetation;  
#' Croplands: Crops;  
#' Wetlands: Natural water, Artifical water, Wetland vegetation;  
#' Settlement: Housing Structure, Infrastructure building, Roads and lots,
#'             urban vegetation;  
#' Other Lands: Barren, Snow/Ice;  

#' ### Reclass table into classes using case_when and dplyr. 

#+ Do Reclass
# Adding the Level 2 Classes. 
reclassedTime1 <- addLevel2(time1)
reclassedTime2 <- addLevel2(time2)

#' #### Level 1 LULC Conversions:
#' Forest Lands = Primary, Secondary, Plantation, Mangrove;  
#' Grasslands = Herbland, Shrubland, Paramo;  
#' Croplands = Cropland;  
#' Wetlands = Aritifical Water, Natural Water;  
#' Settlements = Settlement, Infrastructure;  
#' Other Lands = Glacial, Non-vegetated, Other, Mosaic;  
#' No Data = No Data;  

#+ Adding the Level one classes.
# Add level one classes. 
reclassedTime1 <- addLevel1(reclassedTime1)
reclassedTime2 <- addLevel1(reclassedTime2)

# Begin to assemble output table
finalTable <- metadata
finalTable$T1L1 <- reclassedTime1$LEVEL1
finalTable$T1L2 <- reclassedTime1$LEVEL2
finalTable$T2L1 <- reclassedTime2$LEVEL1
finalTable$T2L2 <- reclassedTime2$LEVEL2
finalTable$changeL1 <- finalTable$T1L1 != finalTable$T2L1
finalTable$changeL2 <- finalTable$T1L2 != finalTable$T2L2


#strip out No_Data entries.
toRemove <- which(finalTable$T1L2 == "No_Data" | finalTable$T2L2 == "No_Data")
if (length(toRemove) > 0) {
	finalTable <- finalTable[-toRemove]
}

# produce final classes
finalTable <- addFinal(finalTable)

#' The SEPAL stratified estimator tool works with integer classes. 
#' However, the data have been imported and prepared as characters. 
#' This section is for converting map values and validation values to integer 
#' values, and will export a csv file that can be uploaded into SEPAL. 

#+ ConvertoClassesandFactors

# Convert to factors. The levels need to be properly set. For the final numeric
# codes to match those of the map, they need to be in the same order as those
# of the map. 
refLevels <- c("Non-vegetated", "Artificial_Water", "Primary_Forest", 
							 "Cropland", "Secondary_Forest", "Infrastructure", 
							 "Natural_Water", "Paramo", "Mangrove", "Plantation_Forest", 
							 "Shrubland", "Herbland", "Settlement", "Glacier",
							 "Forest_Lands", "Grasslands", "Settlements", "Wetlands", 
							 "Other_Lands", 
							 "FC", "FG", "FS", "FW", "CG", "CF", "CS", "GC", "GF", "GS", 
							 "WC", "WS", "OS", "Catchall")

# Add the factors to the table
finalTable$reference <- factor(finalTable$refClass, refLevels)
finalTable$predicted <- factor(finalTable$MapClass, refLevels)

# Convert factors to integers. Subtracting one as GEE begins with 0. 
finalTable$reference <- as.numeric(finalTable$reference) - 1
finalTable$predicted <- as.numeric(finalTable$predicted) - 1

# Export table for upload to SEPAL
write_csv(finalTable, "data/finalTable.csv")

#' ### Importing class area data.  
#' Import class data, reformat the feature properties to make a tidy export.
#' Earth Engine exports a set of lists, with numbers formatted in a slightly
#' odd manner. The lines below convert this into a clean set of areas and 
#' associated classes. 

#+ AreaData
#import area dta exported by GEE.
rawAreas <- read_csv("data/Class_Areas.csv")

#clean up GEE formatting
areas <- gsub("[", "", rawAreas$area, fixed = TRUE) %>% 
	gsub("]", "", ., fixed = TRUE) %>% 
	strsplit(., ", ")

#take number strings and split into the base number and an exponent
areas <- areas[[1]]
areaTable <- str_split(areas, "E", simplify = T)
number <- as.numeric(areaTable[,1])
exponent <- as.numeric(areaTable[,2])

#replace missing exponents with 0, so those numbers are multiplied by 1. 
exponent[is.na(exponent)] <- 0
areas <- number * 10 ^ exponent

areaClasses <- gsub("[", "", rawAreas$classes, fixed = TRUE) %>% 
	gsub("]", "", ., fixed = TRUE) %>% 
	strsplit(., ", ")

# make the area table
cleanAreas <- data.frame(areas, areaClasses[[1]])
names(cleanAreas) <- c("area", "class")

# Export table for upload to SEPAL
write_csv(cleanAreas, "data/area_rast.csv")

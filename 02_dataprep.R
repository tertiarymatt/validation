#' ---
#' title: "Data Prep and Exploration"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' Required packages
#+ Packages
library(tidyverse)
library(stringr)
source('00_functions.R')

#' This script is used to import the photo-interpreted points after they have
#' been produced in Collect Earth Online. The process is as follows.  
#' 1. Import all tables.  
#' 1. Clean and shorten class names.  
#' 1. Find dominant point classes.  
#' 1. Convert dominant point classes to LULC classes. 
#' 
#' ### Importing data.  
#' Import raw data, strip out unneeded name components of class fields. 

#+ Inmport Data
ceoTable <- read_csv("data/validation_error_matrix_dev.csv")
summary(ceoTable)
colnames(ceoTable)

# class names need to be pulled from each project.
classes <- colnames(ceoTable[17:35]) %>% 
	str_split(., coll(":"), simplify = TRUE) %>% 
	.[,2] %>% 
	gsub(" ", "_", .) %>% 
	gsub("/", "_", .)

colnames(ceoTable)[17:35] <- classes
colnames(ceoTable)

#' ### Code block for visualizing the outputs of a CEO project, to give 
#' a rough sense of the data. 
#+ Do Visualization

# Make a plot of each class's distribution of values, dropping zeros 
# (which are super abundant)
select(ceoTable, classes) %>% 
	gather(., key = "class", value = "count") %>%  
	filter(., count > 0) %>% 
	ggplot(., aes(count)) + geom_histogram(bins = 10) + 
	facet_wrap(~class, scales = "free_y")

# How many of each class have a cover value of >50%?
select(ceoTable, classes) %>% 
	gather(., key = "class", value = "count") %>%  
	filter(., count > 50) %>% 
	count(., class) %>% 
	qplot(class, n, data = ., geom = "col", 
				main = c("Instances of class cover >50%")) + coord_flip()

# How many of each class have a non-zero cover value of <50%?
select(ceoTable, classes) %>% 
	gather(., key = "class", value = "count") %>%
	filter(., count > 0) %>%
	filter(., count < 50) %>% 
	count(., class) %>% 
	qplot(class, n, data = ., geom = "col", 
				main = c("Instances of non-zero class cover <50%")) + coord_flip()

# Make a plot of each class's instances
select(ceoTable, classes) %>% 
	gather(., key = "class", value = "count") %>%  
	filter(., count > 0) %>%
	count(., class) %>% 
	qplot(class, n, data = ., geom = "col", 
				main = c("Instances of each class")) + coord_flip()


#' ### CEO Point Table Reclassification
#'
#' Use `addTopClasses()`` to take a raw plot table produced by Collect Earth
#' Online, and returns that table with a Primary and Secondary class field
#' added.

#+ Find dominant landcover elements
ceoTable <- addTopClasses(ceoTable, plotfield = 1, flagfield = 6, 
													classfields = c(17:35))

#' Then use primary and/or secondary classes and threshold values to convert to
#' end classification.
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
#' Forest Land: Primary tree, Secondary tree, Plantation tree, Mangrove  
#' Grassland: Herbaceous vegetation/Grass, Shrub vegetation, Paramo vegetation  
#' Croplands: Crops  
#' Wetlands: Natural water, Artifical water, Wetland vegetation  
#' Settlement: Housing Structure, Infrastructure building, Roads and lots,
#'             urban vegetation  
#' Other Lands: Barren, Snow/Ice  
#'
#' ### Level 1 and Level 2 LULC classes
#' Next steps: add code to convert to level 1 and level 2 classes, and test 
#' agreement/consistency at that level. Because the data is collected a finer
#' level of detail than either Level 1 or Level 2, Level 2 is produced first.  
#' 
#' #### Level 2 LULC Thresholds:   
#' Primary Forest = Secondary tree >= 30%  
#' Secondary Forest = Secondary tree >= 30%  
#' Plantation = Plantation tree >= 30%  
#' Mangrove = Mangrove >= 30%  
#' Grass/herbland = Herbaceous veg > 0% & Tree < 30% & Shrub < 30%  
#' Shrubland = Shrub vegetation >= 30%, Tree < 30%  
#' Paramo = Paramo > 0%  
#' Cropland = Crops >= 50%  
#' Water = Natural water >= 50% | Wetland vegetation >= 50%  
#' Settlement = Houses & Commercial >= 30% | Urban vegetation >= 30%  
#' Infrastructure = Roads and Lots >= 30% | Infrastructure building >= 30%  
#' Non-vegetated = barren >= 70%  
#' Glacial = Snow/Ice >= 70%  

#' ### Reclass table into classes using case_when and dplyr. 
#+ Do Reclass
#+ Level2Classes
# Adding the Level 2 Classes. 
reclassed <- addLevel2(ceoTable)

#' #### Level 1 LULC Conversions:
#' Forest Lands = Primary, Secondary, Plantation, Mangrove  
#' Grasslands = Herbland, Shrubland, Paramo  
#' Croplands = Cropland  
#' Wetlands = Aritifical Water, Natural Water  
#' Settlements = Settlement, Infrastructure  
#' Other Lands = Glacial, Non-vegetated, Other, Mosaic  
#' No Data = No Data  

# Adding the Level one classes.
reclassed <- addLevel1(reclassed)
reclassed

#' The SEPAL stratified estimator tool works with integer classes. 
#' However, the data have been imported and prepared as characters. 
#' This section is for converting map values and validation values to integer 
#' values, and will export a csv file that can be uploaded into SEPAL. 

#+ ConvertoClassesandFactors
#Code to convert these classes into factors, for use in building an error matrix.

# convert integers into text
finalTable <- convertToClasses(reclassed)

#strip out No_Data entries.
toRemove <- which(finalTable$LEVEL2 == "No_Data")
finalTable <- finalTable[-toRemove,]

# Convert to factors. The levels need to be properly set. For the final numeric
# codes to match those of the map, they need to be in the same order as those
# of the map. 
refLevels <- c("Non-vegetated", "Artificial_Water", "Primary_Forest", 
							 "Cropland", "Secondary_Forest", "Infrastructure", 
							 "Natural_Water", "Paramo", "Mangrove", "Plantation_Forest", 
							 "Shrubland", "Herbland", "Settlement", "Glacier")

# Add the factors to the table
finalTable$reference <- factor(finalTable$LEVEL2, refLevels)
finalTable$predicted <- factor(finalTable$MapClass, refLevels)

# Convert factors to integers. Subtracting one as GEE begins with 0. 
finalTable$reference <- as.numeric(finalTable$reference) - 1
finalTable$predicted <- as.numeric(finalTable$predicted) - 1

# Export table for upload to SEPAL
write_csv(finalTable, "data/finalTable.csv")
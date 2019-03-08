#' ---
#' title: "Change Data Prep and Exploration"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' Set working directory to where data is being stored.  
#+ setwd
setwd("~/R/projects/validation")

#' Required packages
#' To use the english class functions file, change `source()` to 
#' `00.1_funcitons_en.R`.
#+ Packages
library(tidyverse)
library(stringr)
source('00.1_functions_en.R')
source('00.3_area_est_functions_en.R')

#' This script is used to import the photo-interpreted points for change
#' data (two years of data) after they have been produced in Collect Earth 
#' Online. The process is as follows.  
#' 1. Import all tables.  
#' 1. Join missing data back into tables.  
#' 1. Clean and shorten class names.  
#' 1. Find dominant point classes.  
#' 1. Convert dominant point classes to LULC classes.  
#' 
#' ### Importing data.  
#' Import raw data, strip out unneeded name components of class fields. 

#+ Import and Prepare Data
#Import original samples
stable <- read_csv("data/points_with_strata/Validation_Sample_for_CEO.csv")
change <- read_csv("data/points_with_strata/Validation_Change_Sample_for_CEO.csv")

allPoints <- bind_rows(stable, change)

# add strata classes
allPoints <- convertStrataClasses(allPoints)

#Import table with map classes
extracted <- read_csv("data/reference/extracted_values_from_maps/sampled_map_classes_2016.csv")
extracted

#drop unneeded fields and convert map classes
extracted <- extracted[,c(2,4,5,6,7,8)]
extracted <- convertMapClasses(extracted)
extracted <- convertMapClasses6(extracted)

# Import gathered CEO data, prepare
ceoTable <- read_csv("data/reference/aggregated_CEO_projects/ceo_2016.csv")
colnames(ceoTable)

#create metadata data table. Use the colnames to find them and adjust columns.
metadata <- ceoTable[,1:11]
head(metadata)

metadata <- inner_join(metadata, allPoints, by = c("CENTER_LON" = "LONGITUDE", 
																									"CENTER_LAT" = "LATITUDE"))

metadata <- inner_join(metadata, extracted)

# assemble full table
ceoTable <- inner_join(metadata, ceoTable)

# class names need to be pulled and cleaned up.
colnames(ceoTable)

# create class column object to use in script.
# If the structure of the data changes this MUST be updated.
classCol <- c(23:41)

classes <- colnames(ceoTable[classCol]) %>% 
	str_split(., coll(":"), simplify = TRUE) %>% 
	.[,2] %>% 
	gsub(" ", "_", .) %>% 
	gsub("/", "_", .)

colnames(ceoTable)[classCol] <- classes

#' ### CEO Point Table Reclassification
#'
#' Use `addTopClasses()`` to take a raw plot table produced by Collect Earth
#' Online, and returns that table with a Primary and Secondary class field
#' added.

#+ Find dominant landcover elements
ceoTable <- addTopClasses(ceoTable, plotfield = 1, flagfield = 6, 
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
reclassed <- addLevel2(ceoTable)


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
reclassed <- addLevel1(reclassed)


# Begin to assemble output table
finalTable <- metadata
finalTable$L1 <- reclassed$LEVEL1
finalTable$L2 <- reclassed$LEVEL2

#strip out No_Data entries.
toRemove <- which(finalTable$L2 == "No_Data" | finalTable$L1 == "No_Data")
if (length(toRemove) > 0) {
	finalTable <- finalTable[-toRemove,]
}

#' The data have been imported and prepared as characters. 
#' This section is for converting map values and validation values to integer 
#' values, and will export a csv file that can be used for further analysis. 

#+ ConvertoClassesandFactors

# Convert to factors. The levels need to be properly set. For the final numeric
# codes to match those of the map, they need to be in the same order as those
# of the map. 
refLevels <- c("Settlement", "Infrastructure", "Barren", "Glacier", 
							 "Natural_Water", "Artificial_Water", "Primary_Forest", 
							 "Plantation_Forest", "Mangrove", "Secondary_Forest",
							 "Cropland", "Paramos", "Shrubland", "Herbland")

ref6Levels <- c("Forest_Lands", "Grasslands", "Croplands", "Wetlands",
							 "Settlements", "Other_Lands")

# Add the factors to the table
finalTable$reference <- factor(finalTable$L2, refLevels)
finalTable$reference6 <- factor(finalTable$L1, ref6Levels)
finalTable$predicted <- factor(finalTable$MapClass, refLevels)
finalTable$predicted6 <- factor(finalTable$MapClass6, ref6Levels)
finalTable$StrataClass <- factor(finalTable$StrataClass, refLevels)

# Convert factors to integers. 
finalTable$refint <- as.numeric(finalTable$reference)
finalTable$ref6int <- as.numeric(finalTable$reference6)
finalTable$predint <- as.numeric(finalTable$predicted)
finalTable$pred6int <- as.numeric(finalTable$predicted6)
finalTable$strataint <- as.numeric(finalTable$StrataClass)

# Export table for upload to SEPAL
write_csv(finalTable, "data/reference/prepared_data/finalTable_2016.csv")

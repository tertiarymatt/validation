#' ---
#' title: "Accuracy Assessment and Area Estimation, Six Class"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#cleanup workspace
rm(list = ls())

#setup
library(tidyverse)
source("00.1_functions_en.R")
source("00.3_area_est_functions_en.R")

#' ### Necessary data  
#' **ceoTable** Outputs of all ceo projects, collated, and unifed with original
#' point metadata. Produced in script `02_change_dataprep.R`.  
#' 
#' **orig_strata** Vector with numeric codes representing the original 
#' stratification of each sample.  
#' 
#' **ref_label** Vector with numeric codes representing the reference label 
#' for that year/map, for each sample.  
#' 
#' **map_label** Vector with numeric codes representing the map labels, for
#' each sample.  
#' 
#' **strata_totals** Dataframe with two columns and number of rows equal to 
#' the total number of classes in the original strata. The first column must 
#' have the same codes found in the original stratification and the second must 
#' have the total number of PIXELS of each class in that original strata map.  
#' 
#' **sample_totals** Dataframe with two columns and number of rows equal to 
#' the total number of classes in the original strata. The first column must 
#' have the same codes found in the original stratification, and the second must
#' have the total number of SAMPLE UNITS of each class collected from that 
#' original strata map.  
#' 
#' **rfcodes** Vector with numeric values representing the reference codes
#' present in ALL of the periods.  


#' ###Importing Data  
#' Importing the full data from the output file. 
completeData <- read_csv("data/reference/complete/finalTable.csv")

#' ### Create **orig_strata**  
#' Vector with numeric codes representing the original stratification of each 
#' sample.  
#' 
#+ origstrata
orig_strata <- completeData$strataint

#' ### Create **ref_label**   
#' Vector with numeric codes representing the reference label for that year/map,
#'  for each sample.  
#+ reflabels
ref_label <- completeData$reference6

#' ### Create **map_label**   
#' Vector with numeric codes representing the map labels, for each sample.  
#+ maplabels
map_label <- completeData$predicted6

#' ### Calculating sample_totals  
#' Generate a dataframe with two columns and number of rows equal to 
#' the total number of classes in the original strata. The first column must 
#' have the same codes found in the original stratification, and the second must
#' have the total number of SAMPLE UNITS of each class collected from that 
#' original strata map.  

#+ sampletots
sample_totals <- data.frame(table(completeData$strataint))
colnames(sample_totals) <- c("strata", "count")

#' ### Set up rfcodes  
#' Vector with numeric values representing the reference codes
#' present in ALL of the periods.  
#'
#' Setting rfcodes: These are the classes from the 2016 MAE map, plus mangroves 
#' and change classes derived from 2014/2016 map updating process. 

#+ rfcodes
# here as text, but used to convert to a factor and produce int codes.
areacodes <- c("Area_Poblada", "Infraestructura", "Area_sin_Cobertura_Vegetal", 
							 "Glaciar", "Cuerpo_de_Agua_Natural", "Cuerpo_de_Agua_Artificial", 
							 "Bosque_Nativo", "Plantacion_Forestal", "Manglar", "Cultivo",  
							 "Paramos", "Vegetacion_Arbustiva", "Vegetacion_Herbacea", 
							 "FF", "GG","SS", "WW", "OO",
							 "FC", "FG", "FS", "FW", "CG", "CF", "CS", "GC", "GF", "GS", 
							 "WC", "WS", "OS", "CATCHALL")

rfcodes <- c("Forest_Lands", "Grasslands", "Croplands", "Wetlands", 
								"Settlements", "Other_Lands",
								"FF", "GG","SS", "WW", "OO",
								"FC", "FG", "FS", "FW", "CG", "CF", "CS", "GC", "GF", "GS",
								"WC", "WS", "OS", "Catchall")

#' ### Calculating strata_totals  
#' Import class data, reformat the feature properties to make a tidy export.
#' Earth Engine exports a set of lists, with numbers formatted in a slightly
#' odd manner. The lines below convert this into a clean set of areas and 
#' associated classes. 

#+ StrataTotals
#set pixel size
pixel <- 30
#import area dta exported by GEE.
stableAreas <- read_csv("data/areas/Class_Areas.csv")
changeAreas <- read_csv("data/areas/Change_Class_Areas.csv")

#clean up GEE formatting
stableClasses <- gsub("[", "", stableAreas$classes, fixed = TRUE) %>% 
	gsub("]", "", ., fixed = TRUE) %>% 
	str_split(., coll(", "), simplify = TRUE) %>% 
	gsub(" ", "_", ., fixed = TRUE)
stableClasses[1,]

changeClasses <- gsub("[", "", changeAreas$classes, fixed = TRUE) %>% 
	gsub("]", "", ., fixed = TRUE) %>% 
	str_split(., coll(", "), simplify = TRUE) %>% 
	gsub(" ", "_", ., fixed = TRUE)
changeClasses[1,]

areaClasses <- c(stableClasses[1,], changeClasses[1,])

stableAreas <- gsub("[", "", stableAreas$area, fixed = TRUE) %>% 
	gsub("]", "", ., fixed = TRUE) %>% 
	strsplit(., ", ")

changeAreas <- gsub("[", "", changeAreas$area, fixed = TRUE) %>% 
	gsub("]", "", ., fixed = TRUE) %>% 
	strsplit(., ", ")

#take number strings and split into the base number and an exponent
areas <- mapply(c, stableAreas, changeAreas)
areaTable <- str_split(areas, "E", simplify = T)
number <- as.numeric(areaTable[,1])
exponent <- as.numeric(areaTable[,2])

#replace missing exponents with 0, so those numbers are multiplied by 1. 
exponent[is.na(exponent)] <- 0
areas <- number * 10 ^ exponent

#convert to factor to get to int codes
areaClasses <- factor(areaClasses, areacodes)
class <- as.numeric(areaClasses)

cleanAreas <- data.frame(class, areas)
names(cleanAreas) <- c("class", "area")

#convert areas to pixel counts
strata_totals <- data.frame(class, areas/pixel^2)
names(strata_totals) <- c("strata", "pixelcount")
strata_totals <- arrange(strata_totals, strata)

#' ### Calculate totarea_pix  
#' Total area of classes, in pixels
#+ totalpixels
totarea_pix <- sum(strata_totals[,2])
totarea_pix

#' ###calcPropsAndVars  
#' Calculate proportions and variances for area and accuracies calculation per 
#' original strata for a given year/map.  
#+ propsvars

propsAndVars <- calcPropsAndVars(orig_strata, ref_label, map_label, 
																 strata_totals, sample_totals, rfcodes)


#' ###calcPropSE  
#' Function to calculate std error of unbiased area proportions of reference
#' classes for a given year/map  
#+ propse

propSE <- calcPropSE(strata_totals, sample_totals, propsAndVars$ref_var, rfcodes, 
										 totarea_pix)

#' ###calcUnbiasedArea  
#' Function to calculate unbiased area, confidence interval and 
#' margin of error.  
#+ unbiasedarea
unArea <- calcUnbiasedArea(totarea_pix, propsAndVars$class_prop, propSE, pixel)

#' ###calcAccuracies  
#' Function to calculate accuracies and their 95% confidence intervals.  
#+ accuracies

accurates <- calcAccuracies(strata_totals, sample_totals, rfcodes, totarea_pix, 
														propsAndVars)
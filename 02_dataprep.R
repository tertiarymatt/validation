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

#' ### Code block for visualizing the outputs of a CEO project. 
#+ Do Visualization

# Inmport Data
ceoTable <- read_csv("data/ceo-prueba-de-clase-plot-data-2018-12-20.csv")
summary(ceoTable)
colnames(ceoTable)

# class names need to be pulled from each project.
classes <- colnames(ceoTable[23:39]) %>% 
	str_split(., coll(":"), simplify = TRUE) %>% 
	.[,2]

colnames(ceoTable)[23:39] <- classes

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
													classfields = c(23:39))

#' Then use primary and/or secondary classes and threshold values to convert to
#' end classification.
#'
#' Point class list (ES): 
#' Arbol nativo, Arbol de rebrote, Arbol de plantacion, Arbol de Mangle
#' Vegetacion herbecea/pastos, Vegetacion arbustiva, Vegetacion de paramo
#' Cultivos Agua natural, Agua Artificial, Vegetacion de humedales Estructura de
#' vivienda, Construccion de infraestructura, Carreteras y lotes, Vegetacion de
#' asentamientos 
#' Suelo desnudo, Nieve/Hielo

#' Point class list (EN): 
#' Forest Land: Native tree, Regrowth tree, Plantation tree, Mangrove 
#' Grassland: Herbaceous vegetation/Grass, Shrub vegetation, Paramo vegetation
#' Croplands: Crops
#' Wetlands: Natural water, Artifical water, Wetland vegetation
#' Settlement: Housing Structure, Infrastructure building, Roads and lots,
#'             urban vegetation
#' Other Lands: Barren, Snow/Ice

#' Thresholds:
#' Primary Forest = Secondary tree >= 30%
#' Secondary Forest = Secondary tree >= 30%
#' Plantation = Plantation tree >= 30% 
#' Mangrove = Mangrove >= 30% 
#' Grass/herbland = Herbaceous veg > 0% & Tree < 30% & Shrub < 30% 
#' Shrubland = Shrub vegetation >= 30%, Tree < 30%
#' Paramo = Paramo > 0%
#' Cropland = Crops >= 50%
#' Water = Natural water >= 50% | Wetland vegetation >= 50%
#' Settlement = Houses & Commercial >= 50% | Urban vegetation >= 50%
#' Infrastructure = Roads and Lots >= 50% | Infrastructure building >= 50%
#' Non-vegetated = barren >= 75%
#' Glacial = Snow/Ice >= 75%

#' ### Reclass table into classes using case_when and dplyr. 
#+ Do Reclass
require("dplyr")
reclassed <- ceoTable %>% 
	mutate(
		Cover = case_when(
			Primary == "PRIMARY_TREE" & PRIMARY_TREE >= 30 ~ "Primary_Forest",
			Primary == "SECONDARY_TREE" & SECONDARY_TREE >= 30 ~ "Secondary_Forest",
			Primary == "PLANTATION_TREE" & PLANTATION_TREE >= 30 ~ "Plantation_Forest",
			Primary == "MANGROVE" & MANGROVE >= 30 ~ "Mangrove",
			Primary == "HERBACEOUS_GRASS_VEGETATION" & HERBACEOUS_GRASS_VEGETATION 
				>= 30 ~ "Herbland",
			Primary == "SHRUB_VEGETATION" & SHRUB_VEGETATION >= 30 ~ "Shrubland",
			Primary == "PARAMO_VEGETATION" & PARAMO_VEGETATION >= 0 ~ "Paramo",
			Primary == "CROPS" & CROPS >= 50 ~ "Cropland",
			Primary == "NATURAL_WATER" & NATURAL_WATER + 
				WETLAND_VEGETATION >= 50 ~  "Natural_Water",
			Primary == "ARTIFICIAL_WATER" & ARTIFICIAL_WATER + 
				WETLAND_VEGETATION >= 50 ~  "Artificial_Water",
			Primary == "WETLAND_VEGETATION" & WETLAND_VEGETATION >= 50 
				~ "Natural_Water"
			TRUE ~ as.character(Cover)
		)
	)

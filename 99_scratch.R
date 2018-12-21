#' ---
#' title: "Scratch Development script"
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
#' Arbol nativo, Arbol de plantacion, Arbol de Mangle
#' Vegetacion herbecea/pastos, Vegetacion arbustiva, Vegetacion de paramo
#' Cultivos Agua natural, Agua Artificial, Vegetacion de humedales Estructura de
#' vivienda, Construccion de infraestructura, Carreteras y lotes, Vegetacion de
#' asentamientos 
#' Suelo desnudo, Nieve/Hielo

#' Point class list (EN): 
#' Forest Land: Native tree, Plantation tree, Mangrove 
#' Grassland: Herbaceous vegetation/Grass, Shrub vegetation, Paramo vegetation
#' Croplands: Crops
#' Wetlands: Natural water, Artifical water, Wetland vegetation
#' Settlement: Housing Structure, Infrastructure building, Roads and lots,
#'             urban vegetation
#' Other Lands: Barren, Snow/Ice

#' Thresholds:
#' Forest = Native tree >= 30% 
#' Plantation = Plantation tree >= 30% 
#' Mangrove = Mangrove >= 30% 
#' Grass/herbland = Herbaceous veg > 0% & Tree < 30% & Shrub < 30% 
#' Shrubland = Shrub vegetation >= 30%, Tree < 30%
#' Paramo = Paramo > 0%
#' Cropland = Crops >= 50%
#' Natural Water = Natural water >= 50% | Wetland vegetation >= 50%
#' Artificial water = Aritifical water >= 50%
#' Settlement = Houses & Commercial >= 50% | Urban vegetation >= 50%
#' Infrastructure = Roads and Lots >= 50% | Infrastructure building >= 50%
#' Non-vegetated = barren >= 75%
#' Glacial = Snow/Ice >= 75%

#' ### Reclass table into classes using case_when and dply. 
#+ Do Reclass
require("dplyr")
reclassed <- ceoTable %>% 
  mutate(
    Primary = case_when(
      Primary == "FOREST_TREE" & FOREST_TREE >= 50 ~ "CLOSED FOREST",
      Primary == "FOREST_TREE" & FOREST_TREE < 50 ~ "OPEN FOREST",
      TRUE ~ as.character(Primary)
    )
  )


#' After reclass, need to use extracted "true values" to build a confusion
#' matrix, using `confusionMatrix()` from `package::caret`. 
#' Note that columns of responses need to be set up as factors. 

#'+ make a confusionMatrix

#' Then need to conver the confusion matrix into an area-based version. 
#' Will need pixel counts and so forth for this. 

#'+ make confusionMatrix area-based

#' Using new area-based confustion matrix, calculate accuracy statistics, and
#' standard errors.

#'+ tidy confusionMatrix, extract statistics



#+ Material from other projects to build on




#+ Fuzzy table builder

#' Single pixel example from Stehman, S. V., Arora, M. K., Kasetkasem, T., &
#' Varshney, P. K. (2007). Estimation of Fuzzy Error Matrix Accuracy Measures
#' Under Stratified Random Sampling. Photogrammetric Engineering & Remote
#' Sensing, 73(2), 165???173. https://doi.org/10.14358/PERS.73.2.165

#+ Single Pixel Example
r <- c(0.3, 0.2, 0.5)
c <- c(0.5, 0.2, 0.3)

#expand from single pixel example

rT <- matrix(nrow = 3, ncol = 3, c(0.3, 0.2, 0.5, 0.3, 0.4, 0.3, 0.4, 0.4, 0.2), byrow = T)
cT <- matrix(nrow = 3, ncol = 3, c(0.5, 0.2, 0.3, 0.3, 0.3, 0.4, 0.3, 0.4, 0.3), byrow = T)

cNames <- c("Forest", "Urban", "Other")

#establish table
fuzzyTable <- matrix(nrow=length(cNames)+1, ncol=length(cNames)+1, 0)

for(i in 1:nrow(cT)){
  r <- rT[i,]
  c <- cT[i,]
  
  #Build a single pixel table
  fuzzyPixel <- matrix(nrow=length(c)+1, ncol=length(r)+1)
  fuzzyPixel[length(cNames)+1,] <- c(r, 0)
  fuzzyPixel[,length(cNames)+1] <- c(c, sum(c))
  
  for(m in 1:length(cNames)){
    for(n in 1:length(cNames)){
      fuzzyPixel[m,n] <- min(r[n], c[m])
    }
  }
  
  rownames(fuzzyPixel) <- c(cNames, "Grade")
  colnames(fuzzyPixel)<- c(cNames, "Grade")
  fuzzyPixel
  
  #add tables
  fuzzyTable <- fuzzyTable + fuzzyPixel
}

fuzzyTable


#### Add in more specifics, closed open, etc ----
valid1 <- read_csv("data/ceo-myanmar-assembly-validation-plot-data-2018-06-21.csv")
validClasses <- topClasses(valid1, plotfield = 1, flagfield = 6, classfields = c(11:30))

unique(validClasses$Primary)

### Basic structure of replacement below, needs to be tailored to the cutoffs for various classes
thing <- validClasses

### Rewrite that using case_when, can be very comprehensive, much cleaner. 
require("dplyr")
thing <- thing %>% 
  mutate(
    Primary = case_when(
      Primary == "FOREST_TREE" & FOREST_TREE >= 50 ~ "CLOSED FOREST",
      Primary == "FOREST_TREE" & FOREST_TREE < 50 ~ "OPEN FOREST",
      TRUE ~ as.character(Primary)
    )
  )


### convert Myanmar CEO project classes to high level IPCC ----

### The below method uses a look up table, basically, to do a join on the main data. 
### This could also theoretically be done using case_when, but might be more lines of code.

conv <- tibble(Primary = classes, 
               IPCC = c("Forest Land", "Forest Land", "Grassland", "Grassland", "Forest Land", 
                        "Cropland", "Cropland", "Settlements", "Settlements", "Settlements", 
                        "Other", "Other", "Other", "Wetlands", "Wetlands", 
                        "Wetlands", "Forest Land", "Other", "Other", "Other"), 
               managed = c(F, F, F, F, T, T, T, T, T, T, F, F, T, F, F, T, F, F, F, F))

test2 <- left_join(test, conv, by="Primary")


### Myanmar Assembly LC codes: Other, Surface Water, Snow and Ice, Mangrove, Closed forest, 
###                             Open forest, Otherwoodedland, Settlement, Cropland, Wetlands, Grassland

Assemble <- read_csv("data/Myanmar_Assembly_Validation.csv")
Assemble$Mode <- factor(Assemble$Mode, levels=c(0:10),
                        labels = c("Other", "Surface Water", "Snow and Ice", "Mangrove", 
                                   "Closed forest", "Open forest", "Otherwoodedland", "Settlement", 
                                   "Cropland", "Wetlands", "Grassland"))
Assemble$Map_Class <- as.character(Assemble$Mode)
Assemble$Mode <- as.numeric(Assemble$Mode)-1
Assemble <- transform(Assemble, latitude = round(latitude, 8), longitude = round(longitude, 8))
Assemble <- unite(Assemble, "coords", latitude, longitude, sep=", ")

valid1 <- read_csv("data/ceo-myanmar-assembly-validation-plot-data-2018-06-21.csv")
valid1 <- topClasses(valid1, plotfield = 1, flagfield = 6, classfields=c(11:30))
valid1 <- unite(valid1, "coords", CENTER_LAT, CENTER_LON, sep=", ")

joined <- inner_join(valid1, Assemble)
length(unique(joined$PLOT_ID))
length(unique(valid1$PLOT_ID))
select(joined, Primary, Map_Class) %>% data.frame() %>% print()

#### Myanmar new classes, simplify. ----

newData <- cross1 %>% 
  transmute(., 
            PlotID = PLOT_ID,
            Flagged = FLAGGED,
            Forest = FOREST_TREE, 
            Mangrove = MANGROVE,
            Grass = GRASS,
            Shrub = SHRUB_W_WOOD,
            Crop = CROP_PLANTATION_ORCHARD + CROP,
            Rice = PADDY_RICE,
            Impervious = IMPERVIOUS_BUILT_SURFACE,
            BuiltVeg = BUILT_TREE + BUILT_VEGETATION,
            Wetlands = WETLANDS_MUDFLAT_TIDALFLAT + AQUATIC_VEGETATION_FLOODED_FOREST,
            Water = WATER + AQUACULTURE_POND,
            Snow = SNOW_ICE,
            Barren = BARREN_SAND_DUNE + ROCKY_MOUNTAIN + MINING,
            Other = OTHER + UNKNOWN) %>% 
  topClasses(.,plotfield = 1, flagfield = 2, classfields = c(3:15))

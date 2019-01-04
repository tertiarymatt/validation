#' ---
#' title: "Scratch Development script"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' 1. Generate sample in GEE, export
#' 1. Import to CEO, PI. 
#' 1. Initial data from CEO split into two tables, one for each year. 
#' 1. Perform a reclass on the Validation Data
#' 1. Rejoin final classes into new table
#' 1. Create change/stable classes
#' 1. After reclass, need to use extracted "true values" to build a confusion 
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

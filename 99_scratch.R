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

#+ Developing functions for optimizing sample size

# need an area-weighted error matrix
error <- matrix(nrow = 2, ncol = 2, 
                c(0.05, 0.0125, 
                  0.0125, 0.925))

nTotal <- 7200

checkErrorMatrix <- function(errorMatrix = NULL){
  checkRows <- sum(rowSums(errorMatrix))
  checkCols <- sum(colSums(errorMatrix))
  total <- checkRows + checkCols
  message <- ifelse(total == 2, c("Error matrix appears correctly formed."),
         c("Errormatrix is not correctly formed."))
  return(message)
}

optimizeSplit <-  function(errorMatrix){
  checkErrorMatrix(errorMatrix)

  v11 <- (error[1,1] * (sum(error[1,]) - error[1,1])/sum(error[1,])^2)
  
  v21 <- ((error[1,1] * error[2,1]) * (error[2,1]*error[1,2]))/sum(error[,1])^4
  v22 <- ((error[1,1] * error[2,1]) * (error[1,1]*error[2,2]))/sum(error[,1])^4
  
  v31 <- error[1,1] * error[1,2]
  v32 <- error[2,1] * error[2,2]
  
  k1 <-  sum(c(v11, v21, v31))
  k2 <- sum(c(v22, v32))
  
  minV <- ifelse(k1 == 0, k2, ifelse(k2 == 0, k1, min(k1, k2)))
  
  n1p <- k1/minV
  n2p <- sqrt(k2/minV)
  np <-  sum(c(n1p, n2p))
  
  n1 <- round((n1p / np) * nTotal)
  n2 <- round((n2p / np) * nTotal)
}

#+ Material from other projects to build on

#' ### Code block for visualizing the outputs of a CEO project. 
#+ Do Visualization
# Inmport Data
valid1 <- read_csv("data/some_data.csv")

# class names need to be pulled from each project. 
classes <- colnames(valid1)[11:30]

# Make a plot of each class's distribution of vales, dropping zeros (which are super abundant)
select(valid1, classes) %>% gather(., key = "class", value = "count") %>%  filter(., count>0) %>% 
  ggplot(., aes(count)) + geom_histogram(bins=10) + facet_wrap(~class, scales="free_y")

# How many of each class have a cover value of >50%?
select(valid1, classes) %>% 
  gather(., key = "class", value = "count") %>%  
  filter(., count>50) %>% 
  count(., class) %>% 
  qplot(class, n, data = ., geom = "col", main = c("Instances of class cover >50%")) + coord_flip()

# How many of each class have a non-zero cover value of <50%?
select(valid1, classes) %>% 
  gather(., key = "class", value = "count") %>%
  filter(., count>0) %>%
  filter(., count<50) %>% 
  count(., class) %>% 
  qplot(class, n, data = ., geom = "col", main = c("Instances of non-zero class cover <50%")) + coord_flip()

# Make a plot of each class's instances
select(valid1, classes) %>% 
  gather(., key = "class", value = "count") %>%  
  filter(., count>0) %>%
  count(., class) %>% 
  qplot(class, n, data = ., geom = "col", main = c("Instances of each class")) + coord_flip()




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

#+ Processing tables into other classes 
valid1 <- read_csv("data/ceo-myanmar-assembly-validation-plot-data-2018-06-21.csv")
classes <- colnames(valid1)[11:30]

validPlots <- select(valid1, "PLOT_ID", "FLAGGED", classes)


pl <- as_vector(validPlots[1,-1])


#Returns name of most abundant class
classes[which.max(pl)]

n <- length(unique(pl))
classes[which(pl == sort(unique(pl))[n])]

#Returns name of second most abundant class, or if that is zero, the max again. 
ifelse(sort(unique(pl))[n-1] == 0, classes[which.max(pl)], classes[which(pl == sort(unique(pl))[n-1])])

primary <- NULL
secondary <- NULL

for(i in 1:nrow(validPlots)){
  
  if(validPlots$FLAGGED[i] == FALSE){
    
    pl <- validPlots[i,-1]
    pl <- as_vector(pl[-1])
    n <- length(unique(pl))
    
    if(length(which(pl == sort(unique(pl))[n])) == 1){ # Is there a tie?
      primary[i] <- classes[which(pl == sort(unique(pl))[n])]
      secondary[i] <- ifelse(sort(unique(pl))[n-1] == 0, # Does the second highest cover has a score of zero?
                             classes[which.max(pl)], # if so, enter max again
                             classes[which(pl == sort(unique(pl))[n-1])]) #if not enter second highest
    } else { #in case of tie, add the tied classes, primary is just the first field encountered
      tie <- classes[which(pl == sort(unique(pl))[n])]
      paste("Plot", validPlots$PLOT_ID[i], "has a tie, with values", tie[1], "and", tie[2])
      primary[i] <- tie[1]
      secondary[i] <- tie[2]
    }
  }
  else{
    primary[i] <- "FLAGGED"
    secondary[i] <- "FLAGGED"
  }
}

validPlots$Primary <- primary
validPlots$Secondary <- secondary

#### Add in more specifics, closed open, etc ----
valid1 <- read_csv("data/ceo-myanmar-assembly-validation-plot-data-2018-06-21.csv")
validClasses <- topClasses(valid1, plotfield = 1, flagfield = 6, classfields = c(11:30))

unique(validClasses$Primary)

### Basic structure of replacement below, needs to be tailored to the cutoffs for various classes
thing <- validClasses

for(i in 1:nrow(validClasses)){
  if(thing$Primary[i] == "FLAGGED"){
    next()
  } else if(thing$Primary[i] == "FOREST_TREE"){
    thing$Primary[i] <- if_else(thing$FOREST_TREE[i] >= 50, "Closed Forest", "Open Forest")
  } 
}

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

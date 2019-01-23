Change Data Prep and Exploration
================
MS Patterson, <tertiarymatt@gmail.com>
January 22, 2019

Set working directory to where data is being stored.
+ setwd

``` r
setwd("~/R/projects/validation")
```

Required packages

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.8
    ## v tidyr   0.8.2     v stringr 1.3.1
    ## v readr   1.2.1     v forcats 0.3.0

    ## -- Conflicts ----------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(stringr)
source('00_functions.R')
```

This script is used to import the photo-interpreted points for change data (two years of data) after they have been produced in Collect Earth Online. The process is as follows.
1. Import all tables.
1. Clean and shorten class names.
1. Find dominant point classes.
1. Convert dominant point classes to LULC classes. 1. Format for use in SEPAL.
1. Export the data. 1. Export class areas for use with the SAE-Analysis tool.

### Importing data.

Import raw data, strip out unneeded name components of class fields.

``` r
ceoTable <- read_csv("data/ceo-two-time-training-1.3-plot-data-2019-01-16.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   SHAPE = col_character(),
    ##   FLAGGED = col_logical(),
    ##   USER_ID = col_character(),
    ##   ANALYSIS_DURATION = col_logical(),
    ##   COLLECTION_TIME = col_datetime(format = "")
    ## )

    ## See spec(...) for full column specifications.

``` r
colnames(ceoTable)
```

    ##  [1] "PLOT_ID"                     "CENTER_LON"                 
    ##  [3] "CENTER_LAT"                  "SIZE_M"                     
    ##  [5] "SHAPE"                       "FLAGGED"                    
    ##  [7] "ANALYSES"                    "SAMPLE_POINTS"              
    ##  [9] "USER_ID"                     "ANALYSIS_DURATION"          
    ## [11] "COLLECTION_TIME"             "PL_LONGITUDE"               
    ## [13] "PL_LATITUDE"                 "PL_PLOTID"                  
    ## [15] "CLASS"                       "2014:PRIMARY TREE"          
    ## [17] "2014:SECONDARY TREE"         "2014:PLANTATION TREE"       
    ## [19] "2014:MANGROVE"               "2014:HERBACEOUS VEGETATION" 
    ## [21] "2014:SHRUB VEGETATION"       "2014:PARAMO VEGETATION"     
    ## [23] "2014:CROPS"                  "2014:NATURAL WATER"         
    ## [25] "2014:ARTIFICIAL WATER"       "2014:WETLAND VEGETATION"    
    ## [27] "2014:HOUSING STRUCTURE"      "2014:INFRASTRUCTURE"        
    ## [29] "2014:ROADS AND LOTS"         "2014:SETTLEMENT VEGETATION" 
    ## [31] "2014:BARE GROUND"            "2014:SNOW/ICE"              
    ## [33] "2014:OTHER"                  "2014:CLOUDS/UNINTERPRETABLE"
    ## [35] "2016:PRIMARY TREE"           "2016:SECONDARY TREE"        
    ## [37] "2016:PLANTATION TREE"        "2016:MANGROVE"              
    ## [39] "2016:HERBACEOUS VEGETATION"  "2016:SHRUB VEGETATION"      
    ## [41] "2016:PARAMO VEGETATION"      "2016:CROPS"                 
    ## [43] "2016:NATURAL WATER"          "2016:ARTIFICIAL WATER"      
    ## [45] "2016:WETLAND VEGETATION"     "2016:HOUSING STRUCTURE"     
    ## [47] "2016:INFRASTRUCTURE"         "2016:ROADS AND LOTS"        
    ## [49] "2016:SETTLEMENT VEGETATION"  "2016:BARE GROUND"           
    ## [51] "2016:SNOW/ICE"               "2016:OTHER"                 
    ## [53] "2016:CLOUDS/UNINTERPRETABLE"

``` r
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
```

    ##  [1] "PLOT_ID"                     "CENTER_LON"                 
    ##  [3] "CENTER_LAT"                  "SIZE_M"                     
    ##  [5] "SHAPE"                       "FLAGGED"                    
    ##  [7] "ANALYSES"                    "SAMPLE_POINTS"              
    ##  [9] "USER_ID"                     "ANALYSIS_DURATION"          
    ## [11] "COLLECTION_TIME"             "PL_LONGITUDE"               
    ## [13] "PL_LATITUDE"                 "PL_PLOTID"                  
    ## [15] "CLASS"                       "MapClass"                   
    ## [17] "2014:PRIMARY TREE"           "2014:SECONDARY TREE"        
    ## [19] "2014:PLANTATION TREE"        "2014:MANGROVE"              
    ## [21] "2014:HERBACEOUS VEGETATION"  "2014:SHRUB VEGETATION"      
    ## [23] "2014:PARAMO VEGETATION"      "2014:CROPS"                 
    ## [25] "2014:NATURAL WATER"          "2014:ARTIFICIAL WATER"      
    ## [27] "2014:WETLAND VEGETATION"     "2014:HOUSING STRUCTURE"     
    ## [29] "2014:INFRASTRUCTURE"         "2014:ROADS AND LOTS"        
    ## [31] "2014:SETTLEMENT VEGETATION"  "2014:BARE GROUND"           
    ## [33] "2014:SNOW/ICE"               "2014:OTHER"                 
    ## [35] "2014:CLOUDS/UNINTERPRETABLE"

``` r
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
```

    ##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [15] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [29] TRUE TRUE TRUE TRUE TRUE TRUE TRUE

### CEO Point Table Reclassification

Use \`addTopClasses()\`\` to take a raw plot table produced by Collect Earth Online, and returns that table with a Primary and Secondary class field added.

``` r
time1 <- addTopClasses(time1, plotfield = 1, flagfield = 6, 
                                                    classfields = classCol)

time2 <- addTopClasses(time2, plotfield = 1, flagfield = 6, 
                                             classfields = classCol)
```

Then use primary and/or secondary classes and threshold values to convert to end classification.

### Level 1 and Level 2 LULC classes

Next steps: add code to convert to level 1 and level 2 classes, and test agreement/consistency at that level. Because the data is collected a finer level of detail than either Level 1 or Level 2, Level 2 is produced first.

#### Level 2 LULC Thresholds:

Primary Forest = Primary tree &gt;= 30%;
Secondary Forest = Secondary tree &gt;= 30%;
Plantation = Plantation tree &gt;= 30%;
Mangrove = Mangrove &gt;= 30%;
Grass/herbland = Herbaceous veg &gt; 0% & Tree &lt; 30% & Shrub &lt; 30%;
Shrubland = Shrub vegetation &gt;= 30%, Tree &lt; 30%;
Paramo = Paramo &gt; 0%;
Cropland = Crops &gt;= 50%;
Water = Natural water &gt;= 50% | Wetland vegetation &gt;= 50%;
Settlement = Houses & Commercial &gt;= 30% | Urban vegetation &gt;= 30%;
Infrastructure = Roads and Lots &gt;= 30% | Infrastructure building &gt;= 30%;
Non-vegetated = barren &gt;= 70%;
Glacial = Snow/Ice &gt;= 70%;

#### Point class list (ES):

Arbol nativo, Arbol de rebrote, Arbol de plantacion, Arbol de Mangle Vegetacion herbecea/pastos, Vegetacion arbustiva, Vegetacion de paramo Cultivos Agua natural, Agua Artificial, Vegetacion de humedales Estructura de vivienda, Construccion de infraestructura, Carreteras y lotes, Vegetacion de asentamientos Suelo desnudo, Nieve/Hielo

#### Point class list (EN):

Forest Land: Primary tree, Secondary tree, Plantation tree, Mangrove;
Grassland: Herbaceous vegetation/Grass, Shrub vegetation, Paramo vegetation;
Croplands: Crops;
Wetlands: Natural water, Artifical water, Wetland vegetation;
Settlement: Housing Structure, Infrastructure building, Roads and lots, urban vegetation;
Other Lands: Barren, Snow/Ice;
\#\#\# Reclass table into classes using case\_when and dplyr.

``` r
# Adding the Level 2 Classes. 
reclassedTime1 <- addLevel2(time1)
reclassedTime2 <- addLevel2(time2)
```

#### Level 1 LULC Conversions:

Forest Lands = Primary, Secondary, Plantation, Mangrove;
Grasslands = Herbland, Shrubland, Paramo;
Croplands = Cropland;
Wetlands = Aritifical Water, Natural Water;
Settlements = Settlement, Infrastructure;
Other Lands = Glacial, Non-vegetated, Other, Mosaic;
No Data = No Data;

``` r
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
```

The SEPAL stratified estimator tool works with integer classes. However, the data have been imported and prepared as characters. This section is for converting map values and validation values to integer values, and will export a csv file that can be uploaded into SEPAL.

``` r
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
```

### Importing class area data.

Import class data, reformat the feature properties to make a tidy export. Earth Engine exports a set of lists, with numbers formatted in a slightly odd manner. The lines below convert this into a clean set of areas and associated classes.

``` r
#import area dta exported by GEE.
rawAreas <- read_csv("data/Class_Areas.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   `system:index` = col_double(),
    ##   area = col_character(),
    ##   classes = col_character(),
    ##   .geo = col_logical()
    ## )

``` r
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

cleanAreas <- data.frame(areas, areaClasses[[1]])

# Export table for upload to SEPAL
write_csv(cleanAreas, "data/area_rast.csv")
```

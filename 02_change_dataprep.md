Change Data Prep and Exploration
================
MS Patterson, <tertiarymatt@gmail.com>
March 08, 2019

Set working directory to where data is being stored.

``` r
setwd("~/R/projects/validation")
```

Required packages To use the english class functions file, change `source()` to `00.1_funcitons_en.R`.

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.8
    ## v tidyr   0.8.2     v stringr 1.3.1
    ## v readr   1.2.1     v forcats 0.3.0

    ## -- Conflicts ------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(stringr)
source('00.1_functions_en.R')
source('00.3_area_est_functions_en.R')
```

This script is used to import the photo-interpreted points for change data (two years of data) after they have been produced in Collect Earth Online. The process is as follows.
1. Import all tables.
1. Join missing data back into tables.
1. Clean and shorten class names.
1. Find dominant point classes.
1. Convert dominant point classes to LULC classes.

### Importing data.

Import raw data, strip out unneeded name components of class fields.

``` r
#Import original samples
stable <- read_csv("data/points_with_strata/Validation_Sample_for_CEO.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Assigned = col_character(),
    ##   LONGITUDE = col_double(),
    ##   LATITUDE = col_double(),
    ##   PLOTID = col_double(),
    ##   ORIGID = col_character(),
    ##   STRATACLASS = col_double(),
    ##   MAPA = col_character(),
    ##   VALID_FIN = col_character()
    ## )

``` r
change <- read_csv("data/points_with_strata/Validation_Change_Sample_for_CEO.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Assigned = col_character(),
    ##   LONGITUDE = col_double(),
    ##   LATITUDE = col_double(),
    ##   PLOTID = col_double(),
    ##   ORIGID = col_character(),
    ##   STRATACLASS = col_double()
    ## )

``` r
allPoints <- bind_rows(stable, change)

# add strata classes
allPoints <- convertStrataClasses(allPoints)

#Import table with map classes
extracted <- read_csv("data/reference/extracted_values_from_maps/sampled_map_classes_change.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   `system:index` = col_character(),
    ##   CENTER_LAT = col_double(),
    ##   CENTER_LON = col_double(),
    ##   COLLECTION = col_datetime(format = ""),
    ##   FLAGGED = col_logical(),
    ##   MAPCLASS = col_double(),
    ##   PLOT_ID = col_double(),
    ##   USER_ID = col_character(),
    ##   .geo = col_logical()
    ## )

``` r
extracted
```

    ## # A tibble: 1,177 x 9
    ##    `system:index` CENTER_LAT CENTER_LON COLLECTION          FLAGGED
    ##    <chr>               <dbl>      <dbl> <dttm>              <lgl>  
    ##  1 0000341f1881d~     0.0213      -75.9 2019-02-16 02:50:07 FALSE  
    ##  2 000060bfb25bf~     0.128       -76.0 2019-02-17 06:05:28 FALSE  
    ##  3 0000e7a7dc5b8~     0.256       -76.0 2019-02-14 20:20:45 FALSE  
    ##  4 00008118b0fff~     0.329       -76.3 2019-02-16 18:46:04 FALSE  
    ##  5 0000e9873bc8c~     0.151       -76.3 2019-02-16 02:15:52 FALSE  
    ##  6 000020e797dbe~     0.208       -76.3 2019-02-13 17:05:10 FALSE  
    ##  7 0000e3495486d~     0.189       -76.2 2019-02-16 18:36:53 FALSE  
    ##  8 000060beed62a~     0.191       -76.3 2019-02-11 17:31:32 FALSE  
    ##  9 0000bd28c17ec~     0.0234      -76.3 2019-02-11 03:52:34 FALSE  
    ## 10 00008826fe599~     0.0816      -76.3 2019-02-13 19:34:40 FALSE  
    ## # ... with 1,167 more rows, and 4 more variables: MAPCLASS <dbl>,
    ## #   PLOT_ID <dbl>, USER_ID <chr>, .geo <lgl>

``` r
#drop unneeded fields and convert map classes
extracted <- extracted[,c(2,4,5,6,7,8)]
extracted <- convertMapClasses6(extracted)

# Import gathered CEO data, prepare
ceoTable <- read_csv("data/reference/aggregated_CEO_projects/ceo_change.csv")
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

    ##  [1] "PLOT_ID"                          
    ##  [2] "CENTER_LON"                       
    ##  [3] "CENTER_LAT"                       
    ##  [4] "SIZE_M"                           
    ##  [5] "SHAPE"                            
    ##  [6] "FLAGGED"                          
    ##  [7] "ANALYSES"                         
    ##  [8] "SAMPLE_POINTS"                    
    ##  [9] "USER_ID"                          
    ## [10] "ANALYSIS_DURATION"                
    ## [11] "COLLECTION_TIME"                  
    ## [12] "2016 COVER:PRIMARY TREE"          
    ## [13] "2016 COVER:SECONDARY TREE"        
    ## [14] "2016 COVER:PLANTATION TREE"       
    ## [15] "2016 COVER:MANGROVE"              
    ## [16] "2016 COVER:HERBACEOUS VEGETATION" 
    ## [17] "2016 COVER:SHRUB VEGETATION"      
    ## [18] "2016 COVER:PARAMO VEGETATION"     
    ## [19] "2016 COVER:CROPS"                 
    ## [20] "2016 COVER:NATURAL WATER"         
    ## [21] "2016 COVER:ARTIFICIAL WATER"      
    ## [22] "2016 COVER:WETLAND VEGETATION"    
    ## [23] "2016 COVER:HOUSING STRUCTURE"     
    ## [24] "2016 COVER:INFRASTRUCTURE"        
    ## [25] "2016 COVER:ROADS AND LOTS"        
    ## [26] "2016 COVER:SETTLEMENT VEGETATION" 
    ## [27] "2016 COVER:BARE GROUND"           
    ## [28] "2016 COVER:SNOW/ICE"              
    ## [29] "2016 COVER:OTHER"                 
    ## [30] "2016 COVER:CLOUDS/UNINTERPRETABLE"
    ## [31] "2014 COVER:PRIMARY TREE"          
    ## [32] "2014 COVER:SECONDARY TREE"        
    ## [33] "2014 COVER:PLANTATION TREE"       
    ## [34] "2014 COVER:MANGROVE"              
    ## [35] "2014 COVER:HERBACEOUS VEGETATION" 
    ## [36] "2014 COVER:SHRUB VEGETATION"      
    ## [37] "2014 COVER:PARAMO VEGETATION"     
    ## [38] "2014 COVER:CROPS"                 
    ## [39] "2014 COVER:NATURAL WATER"         
    ## [40] "2014 COVER:ARTIFICIAL WATER"      
    ## [41] "2014 COVER:WETLAND VEGETATION"    
    ## [42] "2014 COVER:HOUSING STRUCTURE"     
    ## [43] "2014 COVER:INFRASTRUCTURE"        
    ## [44] "2014 COVER:ROADS AND LOTS"        
    ## [45] "2014 COVER:SETTLEMENT VEGETATION" 
    ## [46] "2014 COVER:BARE GROUND"           
    ## [47] "2014 COVER:SNOW/ICE"              
    ## [48] "2014 COVER:OTHER"                 
    ## [49] "2014 COVER:CLOUDS/UNINTERPRETABLE"

``` r
#create metadata data table. Use the colnames to find them and adjust columns.
metadata <- ceoTable[,1:11]
head(metadata)
```

    ## # A tibble: 6 x 11
    ##   PLOT_ID CENTER_LON CENTER_LAT SIZE_M SHAPE FLAGGED ANALYSES SAMPLE_POINTS
    ##     <dbl>      <dbl>      <dbl>  <dbl> <chr> <lgl>      <dbl>         <dbl>
    ## 1       1      -77.0     -1.71      30 squa~ FALSE          0            25
    ## 2       2      -77.0     -0.202     30 squa~ FALSE          0            25
    ## 3       3      -79.2     -0.966     30 squa~ TRUE           0            25
    ## 4       4      -79.2      0.344     30 squa~ FALSE          0            25
    ## 5       5      -80.2     -2.41      30 squa~ FALSE          0            25
    ## 6       6      -76.2     -0.474     30 squa~ FALSE          0            25
    ## # ... with 3 more variables: USER_ID <chr>, ANALYSIS_DURATION <lgl>,
    ## #   COLLECTION_TIME <dttm>

``` r
metadata <- left_join(metadata, allPoints, by = c("CENTER_LON" = "LONGITUDE", 
                                                                                                    "CENTER_LAT" = "LATITUDE"))

metadata <- left_join(metadata, extracted)
```

    ## Joining, by = c("PLOT_ID", "CENTER_LAT", "FLAGGED", "USER_ID")

``` r
# Split table into pieces, reassemble into single year tables

#time one plot data, this is the FIRST year in terms of time passing.
time1 <- ceoTable[,31:49]

#time two plot data
time2 <- ceoTable[,12:30]

#add metadata to the time tables.
time1 <- bind_cols(metadata, time1)
time2 <-  bind_cols(metadata, time2)

# class names need to be pulled and cleaned up.
colnames(time1)
```

    ##  [1] "PLOT_ID"                          
    ##  [2] "CENTER_LON"                       
    ##  [3] "CENTER_LAT"                       
    ##  [4] "SIZE_M"                           
    ##  [5] "SHAPE"                            
    ##  [6] "FLAGGED"                          
    ##  [7] "ANALYSES"                         
    ##  [8] "SAMPLE_POINTS"                    
    ##  [9] "USER_ID"                          
    ## [10] "ANALYSIS_DURATION"                
    ## [11] "COLLECTION_TIME"                  
    ## [12] "Assigned"                         
    ## [13] "PLOTID"                           
    ## [14] "ORIGID"                           
    ## [15] "STRATACLASS"                      
    ## [16] "MAPA"                             
    ## [17] "VALID_FIN"                        
    ## [18] "StrataClass"                      
    ## [19] "COLLECTION"                       
    ## [20] "MAPCLASS"                         
    ## [21] "MapClass6"                        
    ## [22] "2014 COVER:PRIMARY TREE"          
    ## [23] "2014 COVER:SECONDARY TREE"        
    ## [24] "2014 COVER:PLANTATION TREE"       
    ## [25] "2014 COVER:MANGROVE"              
    ## [26] "2014 COVER:HERBACEOUS VEGETATION" 
    ## [27] "2014 COVER:SHRUB VEGETATION"      
    ## [28] "2014 COVER:PARAMO VEGETATION"     
    ## [29] "2014 COVER:CROPS"                 
    ## [30] "2014 COVER:NATURAL WATER"         
    ## [31] "2014 COVER:ARTIFICIAL WATER"      
    ## [32] "2014 COVER:WETLAND VEGETATION"    
    ## [33] "2014 COVER:HOUSING STRUCTURE"     
    ## [34] "2014 COVER:INFRASTRUCTURE"        
    ## [35] "2014 COVER:ROADS AND LOTS"        
    ## [36] "2014 COVER:SETTLEMENT VEGETATION" 
    ## [37] "2014 COVER:BARE GROUND"           
    ## [38] "2014 COVER:SNOW/ICE"              
    ## [39] "2014 COVER:OTHER"                 
    ## [40] "2014 COVER:CLOUDS/UNINTERPRETABLE"

``` r
# create class column object to use in script.
# If the structure of the data changes this MUST be updated.
classCol <- c(22:40)

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
    ## [29] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

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
    finalTable <- finalTable[-toRemove,]
}

finalTable <- drop_na(finalTable, c("MAPCLASS"))

# produce final classes
finalTable <- addFinal(finalTable)
finalTable <- addIPCC(finalTable)
```

The data have been imported and prepared as characters. This section is for converting map values and validation values to integer values, and will export a csv file that can be used for further analysis.

``` r
# Convert to factors. The levels need to be properly set. For the final numeric
# codes to match those of the map, they need to be in the same order as those
# of the map. 

refLevels <- c("Settlement", "Infrastructure", "Barren", "Glacier", 
                             "Natural_Water", "Artificial_Water", "Primary_Forest", 
                             "Plantation_Forest", "Mangrove", "Secondary_Forest",
                             "Cropland", "Paramos", "Shrubland", "Herbland",
                             "FF", "GG","SS", "WW", "OO",
                             "FC", "FG", "FS", "FW", "CG", "CF", "CS", "GC", "GF", "GS",
                             "WC", "WS", "OS", "Catchall")

ref6Levels <- c("Forest_Lands", "Grasslands", "Croplands", "Wetlands",
                                "Settlements", "Other_Lands",
                                "FC", "FG", "FS", "CF", "GF", "GC", "Catchall")

# Add the factors to the table
finalTable$reference6 <- factor(finalTable$ref6Class, ref6Levels)
finalTable$predicted6 <- factor(finalTable$MapClass6, ref6Levels)
finalTable$StrataClass <- factor(finalTable$StrataClass, refLevels)

# Convert factors to integers. 
finalTable$ref6int <- as.numeric(finalTable$reference6)
finalTable$pred6int <- as.numeric(finalTable$predicted6)
finalTable$strataint <- as.numeric(finalTable$StrataClass)

# Export table
write_csv(finalTable, "data/reference/prepared_data/finalTable_change.csv")
```

Data Prep and Exploration
================
MS Patterson, <tertiarymatt@gmail.com>
January 17, 2019

Required packages

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.8
    ## v tidyr   0.8.2     v stringr 1.3.1
    ## v readr   1.2.1     v forcats 0.3.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(stringr)
source('00_functions.R')
```

This script is used to import the photo-interpreted points after they have been produced in Collect Earth Online. The process is as follows.
1. Import all tables.
1. Clean and shorten class names.
1. Find dominant point classes.
1. Convert dominant point classes to LULC classes.

### Importing data.

Import raw data, strip out unneeded name components of class fields.

``` r
ceoTable <- read_csv("data/validation_error_matrix_dev.csv")
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
summary(ceoTable)
```

    ##     PLOT_ID         CENTER_LON       CENTER_LAT          SIZE_M  
    ##  Min.   :  1.00   Min.   :-80.99   Min.   :-4.4231   Min.   :30  
    ##  1st Qu.: 28.00   1st Qu.:-79.83   1st Qu.:-2.6064   1st Qu.:30  
    ##  Median : 55.50   Median :-78.93   Median :-1.8533   Median :30  
    ##  Mean   : 69.25   Mean   :-78.97   Mean   :-1.7867   Mean   :30  
    ##  3rd Qu.:110.25   3rd Qu.:-78.34   3rd Qu.:-0.6755   3rd Qu.:30  
    ##  Max.   :165.00   Max.   :-75.59   Max.   : 1.3180   Max.   :30  
    ##     SHAPE            FLAGGED           ANALYSES SAMPLE_POINTS
    ##  Length:220         Mode :logical   Min.   :0   Min.   :25   
    ##  Class :character   FALSE:187       1st Qu.:0   1st Qu.:25   
    ##  Mode  :character   TRUE :33        Median :0   Median :25   
    ##                                     Mean   :0   Mean   :25   
    ##                                     3rd Qu.:0   3rd Qu.:25   
    ##                                     Max.   :0   Max.   :25   
    ##    USER_ID          ANALYSIS_DURATION COLLECTION_TIME              
    ##  Length:220         Mode:logical      Min.   :2018-12-30 03:12:30  
    ##  Class :character   NA's:220          1st Qu.:2018-12-31 00:22:54  
    ##  Mode  :character                     Median :2019-01-02 13:04:37  
    ##                                       Mean   :2019-01-02 11:40:06  
    ##                                       3rd Qu.:2019-01-03 20:46:30  
    ##                                       Max.   :2019-01-07 18:21:16  
    ##   PL_LONGITUDE     PL_LATITUDE        PL_PLOTID        PL_FID_REFDTA  
    ##  Min.   :-80.99   Min.   :-4.4231   Min.   :    1.00   Min.   :   76  
    ##  1st Qu.:-79.83   1st Qu.:-2.6064   1st Qu.:   55.75   1st Qu.: 7842  
    ##  Median :-78.93   Median :-1.8533   Median :  110.50   Median :22649  
    ##  Mean   :-78.97   Mean   :-1.7867   Mean   : 5280.06   Mean   :20777  
    ##  3rd Qu.:-78.34   3rd Qu.:-0.6755   3rd Qu.:  305.25   3rd Qu.:31750  
    ##  Max.   :-75.59   Max.   : 1.3180   Max.   :41851.00   Max.   :41936  
    ##      CLASS        LAND COVER:PRIMARY TREE LAND COVER:SECONDARY TREE
    ##  Min.   : 0.000   Min.   :  0.000         Min.   :  0.000          
    ##  1st Qu.: 2.000   1st Qu.:  0.000         1st Qu.:  0.000          
    ##  Median : 9.000   Median :  0.000         Median :  0.000          
    ##  Mean   : 7.982   Mean   :  9.745         Mean   :  3.509          
    ##  3rd Qu.:13.000   3rd Qu.:  0.000         3rd Qu.:  0.000          
    ##  Max.   :15.000   Max.   :100.000         Max.   :100.000          
    ##  LAND COVER:PLANTATION TREE LAND COVER:MANGROVE
    ##  Min.   :  0.0              Min.   :  0.000    
    ##  1st Qu.:  0.0              1st Qu.:  0.000    
    ##  Median :  0.0              Median :  0.000    
    ##  Mean   :  1.4              Mean   :  3.382    
    ##  3rd Qu.:  0.0              3rd Qu.:  0.000    
    ##  Max.   :100.0              Max.   :100.000    
    ##  LAND COVER:HERBACEOUS/GRASS VEGETATION LAND COVER:SHRUB VEGETATION
    ##  Min.   :  0.00                         Min.   :  0.0              
    ##  1st Qu.:  0.00                         1st Qu.:  0.0              
    ##  Median :  0.00                         Median :  0.0              
    ##  Mean   : 15.27                         Mean   : 16.8              
    ##  3rd Qu.:  8.00                         3rd Qu.:  9.0              
    ##  Max.   :100.00                         Max.   :100.0              
    ##  LAND COVER:PARAMO VEGETATION LAND COVER:CROPS  LAND COVER:NATURAL WATER
    ##  Min.   :  0.000              Min.   :  0.000   Min.   :  0.000         
    ##  1st Qu.:  0.000              1st Qu.:  0.000   1st Qu.:  0.000         
    ##  Median :  0.000              Median :  0.000   Median :  0.000         
    ##  Mean   :  2.055              Mean   :  4.109   Mean   :  4.382         
    ##  3rd Qu.:  0.000              3rd Qu.:  0.000   3rd Qu.:  0.000         
    ##  Max.   :100.000              Max.   :100.000   Max.   :100.000         
    ##  LAND COVER:ARTIFICIAL WATER LAND COVER:WETLAND VEGETATION
    ##  Min.   :  0.000             Min.   :  0.0000             
    ##  1st Qu.:  0.000             1st Qu.:  0.0000             
    ##  Median :  0.000             Median :  0.0000             
    ##  Mean   :  2.982             Mean   :  0.9455             
    ##  3rd Qu.:  0.000             3rd Qu.:  0.0000             
    ##  Max.   :100.000             Max.   :100.0000             
    ##  LAND COVER:HOUSING STRUCTURE LAND COVER:INFRASTRUCTURE
    ##  Min.   : 0.0000              Min.   :  0.0000         
    ##  1st Qu.: 0.0000              1st Qu.:  0.0000         
    ##  Median : 0.0000              Median :  0.0000         
    ##  Mean   : 0.7273              Mean   :  0.5091         
    ##  3rd Qu.: 0.0000              3rd Qu.:  0.0000         
    ##  Max.   :60.0000              Max.   :100.0000         
    ##  LAND COVER:ROADS AND LOTS LAND COVER:SETTLEMENT VEGETATION
    ##  Min.   : 0.000            Min.   :  0.000                 
    ##  1st Qu.: 0.000            1st Qu.:  0.000                 
    ##  Median : 0.000            Median :  0.000                 
    ##  Mean   : 1.855            Mean   :  1.727                 
    ##  3rd Qu.: 0.000            3rd Qu.:  0.000                 
    ##  Max.   :72.000            Max.   :100.000                 
    ##  LAND COVER:BARE GROUND LAND COVER:SNOW/ICE LAND COVER:OTHER
    ##  Min.   :  0.000        Min.   :  0.000     Min.   :0       
    ##  1st Qu.:  0.000        1st Qu.:  0.000     1st Qu.:0       
    ##  Median :  0.000        Median :  0.000     Median :0       
    ##  Mean   :  4.255        Mean   :  3.745     Mean   :0       
    ##  3rd Qu.:  0.000        3rd Qu.:  0.000     3rd Qu.:0       
    ##  Max.   :100.000        Max.   :100.000     Max.   :0       
    ##  LAND COVER:CLOUDS/UNINTERPRETABLE
    ##  Min.   :  0.0                    
    ##  1st Qu.:  0.0                    
    ##  Median :  0.0                    
    ##  Mean   :  7.6                    
    ##  3rd Qu.:  0.0                    
    ##  Max.   :100.0

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
    ## [12] "PL_LONGITUDE"                          
    ## [13] "PL_LATITUDE"                           
    ## [14] "PL_PLOTID"                             
    ## [15] "PL_FID_REFDTA"                         
    ## [16] "CLASS"                                 
    ## [17] "LAND COVER:PRIMARY TREE"               
    ## [18] "LAND COVER:SECONDARY TREE"             
    ## [19] "LAND COVER:PLANTATION TREE"            
    ## [20] "LAND COVER:MANGROVE"                   
    ## [21] "LAND COVER:HERBACEOUS/GRASS VEGETATION"
    ## [22] "LAND COVER:SHRUB VEGETATION"           
    ## [23] "LAND COVER:PARAMO VEGETATION"          
    ## [24] "LAND COVER:CROPS"                      
    ## [25] "LAND COVER:NATURAL WATER"              
    ## [26] "LAND COVER:ARTIFICIAL WATER"           
    ## [27] "LAND COVER:WETLAND VEGETATION"         
    ## [28] "LAND COVER:HOUSING STRUCTURE"          
    ## [29] "LAND COVER:INFRASTRUCTURE"             
    ## [30] "LAND COVER:ROADS AND LOTS"             
    ## [31] "LAND COVER:SETTLEMENT VEGETATION"      
    ## [32] "LAND COVER:BARE GROUND"                
    ## [33] "LAND COVER:SNOW/ICE"                   
    ## [34] "LAND COVER:OTHER"                      
    ## [35] "LAND COVER:CLOUDS/UNINTERPRETABLE"

``` r
# class names need to be pulled from each project.
classes <- colnames(ceoTable[17:35]) %>% 
    str_split(., coll(":"), simplify = TRUE) %>% 
    .[,2] %>% 
    gsub(" ", "_", .) %>% 
    gsub("/", "_", .)

colnames(ceoTable)[17:35] <- classes
colnames(ceoTable)
```

    ##  [1] "PLOT_ID"                     "CENTER_LON"                 
    ##  [3] "CENTER_LAT"                  "SIZE_M"                     
    ##  [5] "SHAPE"                       "FLAGGED"                    
    ##  [7] "ANALYSES"                    "SAMPLE_POINTS"              
    ##  [9] "USER_ID"                     "ANALYSIS_DURATION"          
    ## [11] "COLLECTION_TIME"             "PL_LONGITUDE"               
    ## [13] "PL_LATITUDE"                 "PL_PLOTID"                  
    ## [15] "PL_FID_REFDTA"               "CLASS"                      
    ## [17] "PRIMARY_TREE"                "SECONDARY_TREE"             
    ## [19] "PLANTATION_TREE"             "MANGROVE"                   
    ## [21] "HERBACEOUS_GRASS_VEGETATION" "SHRUB_VEGETATION"           
    ## [23] "PARAMO_VEGETATION"           "CROPS"                      
    ## [25] "NATURAL_WATER"               "ARTIFICIAL_WATER"           
    ## [27] "WETLAND_VEGETATION"          "HOUSING_STRUCTURE"          
    ## [29] "INFRASTRUCTURE"              "ROADS_AND_LOTS"             
    ## [31] "SETTLEMENT_VEGETATION"       "BARE_GROUND"                
    ## [33] "SNOW_ICE"                    "OTHER"                      
    ## [35] "CLOUDS_UNINTERPRETABLE"

### Code block for visualizing the outputs of a CEO project, to give

a rough sense of the data.

``` r
# Make a plot of each class's distribution of values, dropping zeros 
# (which are super abundant)
select(ceoTable, classes) %>% 
    gather(., key = "class", value = "count") %>%  
    filter(., count > 0) %>% 
    ggplot(., aes(count)) + geom_histogram(bins = 10) + 
    facet_wrap(~class, scales = "free_y")
```

![](02_single_year_dataprep_files/figure-markdown_github/Do%20Visualization-1.png)

``` r
# How many of each class have a cover value of >50%?
select(ceoTable, classes) %>% 
    gather(., key = "class", value = "count") %>%  
    filter(., count > 50) %>% 
    count(., class) %>% 
    qplot(class, n, data = ., geom = "col", 
                main = c("Instances of class cover >50%")) + coord_flip()
```

![](02_single_year_dataprep_files/figure-markdown_github/Do%20Visualization-2.png)

``` r
# How many of each class have a non-zero cover value of <50%?
select(ceoTable, classes) %>% 
    gather(., key = "class", value = "count") %>%
    filter(., count > 0) %>%
    filter(., count < 50) %>% 
    count(., class) %>% 
    qplot(class, n, data = ., geom = "col", 
                main = c("Instances of non-zero class cover <50%")) + coord_flip()
```

![](02_single_year_dataprep_files/figure-markdown_github/Do%20Visualization-3.png)

``` r
# Make a plot of each class's instances
select(ceoTable, classes) %>% 
    gather(., key = "class", value = "count") %>%  
    filter(., count > 0) %>%
    count(., class) %>% 
    qplot(class, n, data = ., geom = "col", 
                main = c("Instances of each class")) + coord_flip()
```

![](02_single_year_dataprep_files/figure-markdown_github/Do%20Visualization-4.png)

### CEO Point Table Reclassification

Use \`addTopClasses()\`\` to take a raw plot table produced by Collect Earth Online, and returns that table with a Primary and Secondary class field added.

``` r
ceoTable <- addTopClasses(ceoTable, plotfield = 1, flagfield = 6, 
                                                    classfields = c(17:35))
```

Then use primary and/or secondary classes and threshold values to convert to end classification.

#### Point class list (ES):

Arbol nativo, Arbol de rebrote, Arbol de plantacion, Arbol de Mangle Vegetacion herbecea/pastos, Vegetacion arbustiva, Vegetacion de paramo Cultivos Agua natural, Agua Artificial, Vegetacion de humedales Estructura de vivienda, Construccion de infraestructura, Carreteras y lotes, Vegetacion de asentamientos Suelo desnudo, Nieve/Hielo

#### Point class list (EN):

Forest Land: Primary tree, Secondary tree, Plantation tree, Mangrove
Grassland: Herbaceous vegetation/Grass, Shrub vegetation, Paramo vegetation
Croplands: Crops
Wetlands: Natural water, Artifical water, Wetland vegetation
Settlement: Housing Structure, Infrastructure building, Roads and lots, urban vegetation
Other Lands: Barren, Snow/Ice

### Level 1 and Level 2 LULC classes

Next steps: add code to convert to level 1 and level 2 classes, and test agreement/consistency at that level. Because the data is collected a finer level of detail than either Level 1 or Level 2, Level 2 is produced first.

#### Level 2 LULC Thresholds:

Primary Forest = Secondary tree &gt;= 30%
Secondary Forest = Secondary tree &gt;= 30%
Plantation = Plantation tree &gt;= 30%
Mangrove = Mangrove &gt;= 30%
Grass/herbland = Herbaceous veg &gt; 0% & Tree &lt; 30% & Shrub &lt; 30%
Shrubland = Shrub vegetation &gt;= 30%, Tree &lt; 30%
Paramo = Paramo &gt; 0%
Cropland = Crops &gt;= 50%
Water = Natural water &gt;= 50% | Wetland vegetation &gt;= 50%
Settlement = Houses & Commercial &gt;= 30% | Urban vegetation &gt;= 30%
Infrastructure = Roads and Lots &gt;= 30% | Infrastructure building &gt;= 30%
Non-vegetated = barren &gt;= 70%
Glacial = Snow/Ice &gt;= 70%
\#\#\# Reclass table into classes using case\_when and dplyr.

``` r
# Adding the Level 2 Classes. 
reclassed <- addLevel2(ceoTable)
```

#### Level 1 LULC Conversions:

Forest Lands = Primary, Secondary, Plantation, Mangrove
Grasslands = Herbland, Shrubland, Paramo
Croplands = Cropland
Wetlands = Aritifical Water, Natural Water
Settlements = Settlement, Infrastructure
Other Lands = Glacial, Non-vegetated, Other, Mosaic
No Data = No Data

``` r
# Adding the Level one classes.
reclassed <- addLevel1(reclassed)
reclassed
```

    ## # A tibble: 220 x 39
    ##    PLOT_ID CENTER_LON CENTER_LAT SIZE_M SHAPE FLAGGED ANALYSES
    ##      <dbl>      <dbl>      <dbl>  <dbl> <chr> <lgl>      <dbl>
    ##  1      35      -76.7      0.129     30 squa~ FALSE          0
    ##  2      37      -78.6     -0.767     30 squa~ FALSE          0
    ##  3      39      -78.7     -2.55      30 squa~ FALSE          0
    ##  4      43      -79.7     -2.31      30 squa~ FALSE          0
    ##  5      45      -77.1     -0.558     30 squa~ FALSE          0
    ##  6      47      -79.3     -4.25      30 squa~ FALSE          0
    ##  7      49      -78.8     -1.26      30 squa~ FALSE          0
    ##  8      23      -78.6     -1.26      30 squa~ FALSE          0
    ##  9      26      -78.6     -1.26      30 squa~ FALSE          0
    ## 10      27      -78.8     -2.07      30 squa~ FALSE          0
    ## # ... with 210 more rows, and 32 more variables: SAMPLE_POINTS <dbl>,
    ## #   USER_ID <chr>, ANALYSIS_DURATION <lgl>, COLLECTION_TIME <dttm>,
    ## #   PL_LONGITUDE <dbl>, PL_LATITUDE <dbl>, PL_PLOTID <dbl>,
    ## #   PL_FID_REFDTA <dbl>, CLASS <dbl>, PRIMARY_TREE <dbl>,
    ## #   SECONDARY_TREE <dbl>, PLANTATION_TREE <dbl>, MANGROVE <dbl>,
    ## #   HERBACEOUS_GRASS_VEGETATION <dbl>, SHRUB_VEGETATION <dbl>,
    ## #   PARAMO_VEGETATION <dbl>, CROPS <dbl>, NATURAL_WATER <dbl>,
    ## #   ARTIFICIAL_WATER <dbl>, WETLAND_VEGETATION <dbl>,
    ## #   HOUSING_STRUCTURE <dbl>, INFRASTRUCTURE <dbl>, ROADS_AND_LOTS <dbl>,
    ## #   SETTLEMENT_VEGETATION <dbl>, BARE_GROUND <dbl>, SNOW_ICE <dbl>,
    ## #   OTHER <dbl>, CLOUDS_UNINTERPRETABLE <dbl>, Primary <chr>,
    ## #   Secondary <chr>, LEVEL2 <chr>, LEVEL1 <chr>

The SEPAL stratified estimator tool works with integer classes. However, the data have been imported and prepared as characters. This section is for converting map values and validation values to integer values, and will export a csv file that can be uploaded into SEPAL.

``` r
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
```

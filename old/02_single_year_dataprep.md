Data Prep and Exploration
================
MS Patterson, <tertiarymatt@gmail.com>
January 27, 2019

Set working directory to where data is being stored. + setwd

``` r
setwd("~/R/projects/validation")
```

Required packages To use the english class functions file, change `source()` to `00.1_funcitons_en.R`.

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.8
    ## v tidyr   0.8.2     v stringr 1.3.1
    ## v readr   1.2.1     v forcats 0.3.0

    ## -- Conflicts ----------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(stringr)
source('00.2_functions_es.R')
```

This script is used to import the photo-interpreted points after they have been produced in Collect Earth Online. The process is as follows.
1. Import all tables.
1. Clean and shorten class names.
1. Find dominant point classes.
1. Convert dominant point classes to LULC classes.

### Importing data.

Import raw data, strip out unneeded name components of class fields.

``` r
ceoTable <- read_csv("data/ceo-plantilla-de-validacion-plot-data-2019-01-26.csv")
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

    ##     PLOT_ID       CENTER_LON       CENTER_LAT          SIZE_M  
    ##  Min.   :   1   Min.   :-91.62   Min.   :-4.9944   Min.   :30  
    ##  1st Qu.:1501   1st Qu.:-79.56   1st Qu.:-2.2574   1st Qu.:30  
    ##  Median :3000   Median :-78.77   Median :-1.3320   Median :30  
    ##  Mean   :3000   Mean   :-78.78   Mean   :-1.4136   Mean   :30  
    ##  3rd Qu.:4500   3rd Qu.:-77.49   3rd Qu.:-0.3734   3rd Qu.:30  
    ##  Max.   :6000   Max.   :-75.23   Max.   : 1.4519   Max.   :30  
    ##                                                                
    ##     SHAPE            FLAGGED           ANALYSES SAMPLE_POINTS
    ##  Length:6000        Mode :logical   Min.   :0   Min.   :25   
    ##  Class :character   FALSE:6000      1st Qu.:0   1st Qu.:25   
    ##  Mode  :character                   Median :0   Median :25   
    ##                                     Mean   :0   Mean   :25   
    ##                                     3rd Qu.:0   3rd Qu.:25   
    ##                                     Max.   :0   Max.   :25   
    ##                                                              
    ##    USER_ID          ANALYSIS_DURATION COLLECTION_TIME              
    ##  Length:6000        Mode:logical      Min.   :2019-01-26 17:36:23  
    ##  Class :character   NA's:6000         1st Qu.:2019-01-26 17:44:09  
    ##  Mode  :character                     Median :2019-01-26 17:46:51  
    ##                                       Mean   :2019-01-26 17:45:48  
    ##                                       3rd Qu.:2019-01-26 17:48:29  
    ##                                       Max.   :2019-01-26 17:50:00  
    ##                                       NA's   :5975                 
    ##   PL_LONGITUDE     PL_LATITUDE        PL_PLOTID       PL_CLASS     
    ##  Min.   :-91.62   Min.   :-4.9944   Min.   :   0   Min.   : 1.000  
    ##  1st Qu.:-79.56   1st Qu.:-2.2574   1st Qu.:1500   1st Qu.: 4.000  
    ##  Median :-78.77   Median :-1.3320   Median :3000   Median : 4.000  
    ##  Mean   :-78.78   Mean   :-1.4136   Mean   :3000   Mean   : 6.288  
    ##  3rd Qu.:-77.49   3rd Qu.:-0.3734   3rd Qu.:4499   3rd Qu.: 5.000  
    ##  Max.   :-75.23   Max.   : 1.4519   Max.   :5999   Max.   :42.000  
    ##                                                                    
    ##  COBERTURA TERRESTRE:ARBOL PRIMARIO COBERTURA TERRESTRE:ARBOL SECUNDARIO
    ##  Min.   :  0.00000                  Min.   :  0.00000                   
    ##  1st Qu.:  0.00000                  1st Qu.:  0.00000                   
    ##  Median :  0.00000                  Median :  0.00000                   
    ##  Mean   :  0.01667                  Mean   :  0.01667                   
    ##  3rd Qu.:  0.00000                  3rd Qu.:  0.00000                   
    ##  Max.   :100.00000                  Max.   :100.00000                   
    ##                                                                         
    ##  COBERTURA TERRESTRE:ARBOL DE PLANTACION
    ##  Min.   :0e+00                          
    ##  1st Qu.:0e+00                          
    ##  Median :0e+00                          
    ##  Mean   :5e-02                          
    ##  3rd Qu.:0e+00                          
    ##  Max.   :1e+02                          
    ##                                         
    ##  COBERTURA TERRESTRE:ARBOL DE MANGLE
    ##  Min.   :  0.00000                  
    ##  1st Qu.:  0.00000                  
    ##  Median :  0.00000                  
    ##  Mean   :  0.01667                  
    ##  3rd Qu.:  0.00000                  
    ##  Max.   :100.00000                  
    ##                                     
    ##  COBERTURA TERRESTRE:VEGETACION HERBACEA/PASTOS
    ##  Min.   :  0.00000                             
    ##  1st Qu.:  0.00000                             
    ##  Median :  0.00000                             
    ##  Mean   :  0.03333                             
    ##  3rd Qu.:  0.00000                             
    ##  Max.   :100.00000                             
    ##                                                
    ##  COBERTURA TERRESTRE:VEGETACION ARBUSTIVA
    ##  Min.   :  0.00000                       
    ##  1st Qu.:  0.00000                       
    ##  Median :  0.00000                       
    ##  Mean   :  0.01667                       
    ##  3rd Qu.:  0.00000                       
    ##  Max.   :100.00000                       
    ##                                          
    ##  COBERTURA TERRESTRE:VEGETACION DE PARAMO COBERTURA TERRESTRE:CULTIVOS
    ##  Min.   :  0.00000                        Min.   :  0.00000           
    ##  1st Qu.:  0.00000                        1st Qu.:  0.00000           
    ##  Median :  0.00000                        Median :  0.00000           
    ##  Mean   :  0.01667                        Mean   :  0.08333           
    ##  3rd Qu.:  0.00000                        3rd Qu.:  0.00000           
    ##  Max.   :100.00000                        Max.   :100.00000           
    ##                                                                       
    ##  COBERTURA TERRESTRE:AGUA NATURAL COBERTURA TERRESTRE:AGUA ARTIFICIAL
    ##  Min.   :0                        Min.   :0                          
    ##  1st Qu.:0                        1st Qu.:0                          
    ##  Median :0                        Median :0                          
    ##  Mean   :0                        Mean   :0                          
    ##  3rd Qu.:0                        3rd Qu.:0                          
    ##  Max.   :0                        Max.   :0                          
    ##                                                                      
    ##  COBERTURA TERRESTRE:VEGETACION DE HUMEDALES
    ##  Min.   :0                                  
    ##  1st Qu.:0                                  
    ##  Median :0                                  
    ##  Mean   :0                                  
    ##  3rd Qu.:0                                  
    ##  Max.   :0                                  
    ##                                             
    ##  COBERTURA TERRESTRE:ESTRUCTURA DE VIVIENDA
    ##  Min.   :  0.00000                         
    ##  1st Qu.:  0.00000                         
    ##  Median :  0.00000                         
    ##  Mean   :  0.05067                         
    ##  3rd Qu.:  0.00000                         
    ##  Max.   :100.00000                         
    ##                                            
    ##  COBERTURA TERRESTRE:INFRAESTRUCTURA
    ##  Min.   :  0.00000                  
    ##  1st Qu.:  0.00000                  
    ##  Median :  0.00000                  
    ##  Mean   :  0.03333                  
    ##  3rd Qu.:  0.00000                  
    ##  Max.   :100.00000                  
    ##                                     
    ##  COBERTURA TERRESTRE:CARRETERAS Y LOTES
    ##  Min.   : 0.00000                      
    ##  1st Qu.: 0.00000                      
    ##  Median : 0.00000                      
    ##  Mean   : 0.01867                      
    ##  3rd Qu.: 0.00000                      
    ##  Max.   :40.00000                      
    ##                                        
    ##  COBERTURA TERRESTRE:VEGETACION DE ASENTAMIENTOS
    ##  Min.   :0                                      
    ##  1st Qu.:0                                      
    ##  Median :0                                      
    ##  Mean   :0                                      
    ##  3rd Qu.:0                                      
    ##  Max.   :0                                      
    ##                                                 
    ##  COBERTURA TERRESTRE:SUELO DESNUDO COBERTURA TERRESTRE:NIEVE/HIELO
    ##  Min.   : 0.000                    Min.   :  0.00000              
    ##  1st Qu.: 0.000                    1st Qu.:  0.00000              
    ##  Median : 0.000                    Median :  0.00000              
    ##  Mean   : 0.014                    Mean   :  0.01667              
    ##  3rd Qu.: 0.000                    3rd Qu.:  0.00000              
    ##  Max.   :84.000                    Max.   :100.00000              
    ##                                                                   
    ##  COBERTURA TERRESTRE:OTRO COBERTURA TERRESTRE:NUBE/ININTELIGIBLE
    ##  Min.   :  0.00000        Min.   :  0.00000                     
    ##  1st Qu.:  0.00000        1st Qu.:  0.00000                     
    ##  Median :  0.00000        Median :  0.00000                     
    ##  Mean   :  0.01667        Mean   :  0.01667                     
    ##  3rd Qu.:  0.00000        3rd Qu.:  0.00000                     
    ##  Max.   :100.00000        Max.   :100.00000                     
    ## 

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
    ## [15] "PL_CLASS"                                       
    ## [16] "COBERTURA TERRESTRE:ARBOL PRIMARIO"             
    ## [17] "COBERTURA TERRESTRE:ARBOL SECUNDARIO"           
    ## [18] "COBERTURA TERRESTRE:ARBOL DE PLANTACION"        
    ## [19] "COBERTURA TERRESTRE:ARBOL DE MANGLE"            
    ## [20] "COBERTURA TERRESTRE:VEGETACION HERBACEA/PASTOS" 
    ## [21] "COBERTURA TERRESTRE:VEGETACION ARBUSTIVA"       
    ## [22] "COBERTURA TERRESTRE:VEGETACION DE PARAMO"       
    ## [23] "COBERTURA TERRESTRE:CULTIVOS"                   
    ## [24] "COBERTURA TERRESTRE:AGUA NATURAL"               
    ## [25] "COBERTURA TERRESTRE:AGUA ARTIFICIAL"            
    ## [26] "COBERTURA TERRESTRE:VEGETACION DE HUMEDALES"    
    ## [27] "COBERTURA TERRESTRE:ESTRUCTURA DE VIVIENDA"     
    ## [28] "COBERTURA TERRESTRE:INFRAESTRUCTURA"            
    ## [29] "COBERTURA TERRESTRE:CARRETERAS Y LOTES"         
    ## [30] "COBERTURA TERRESTRE:VEGETACION DE ASENTAMIENTOS"
    ## [31] "COBERTURA TERRESTRE:SUELO DESNUDO"              
    ## [32] "COBERTURA TERRESTRE:NIEVE/HIELO"                
    ## [33] "COBERTURA TERRESTRE:OTRO"                       
    ## [34] "COBERTURA TERRESTRE:NUBE/ININTELIGIBLE"

``` r
# class names need to be pulled from each project.
# The classCol variable MUST be updated! 
classCol <- c(16:34)
classes <- colnames(ceoTable[classCol]) %>% 
    str_split(., coll(":"), simplify = TRUE) %>% 
    .[,2] %>% 
    gsub(" ", "_", .) %>% 
    gsub("/", "_", .)

colnames(ceoTable)[classCol] <- classes
colnames(ceoTable)
```

    ##  [1] "PLOT_ID"                     "CENTER_LON"                 
    ##  [3] "CENTER_LAT"                  "SIZE_M"                     
    ##  [5] "SHAPE"                       "FLAGGED"                    
    ##  [7] "ANALYSES"                    "SAMPLE_POINTS"              
    ##  [9] "USER_ID"                     "ANALYSIS_DURATION"          
    ## [11] "COLLECTION_TIME"             "PL_LONGITUDE"               
    ## [13] "PL_LATITUDE"                 "PL_PLOTID"                  
    ## [15] "PL_CLASS"                    "ARBOL_PRIMARIO"             
    ## [17] "ARBOL_SECUNDARIO"            "ARBOL_DE_PLANTACION"        
    ## [19] "ARBOL_DE_MANGLE"             "VEGETACION_HERBACEA_PASTOS" 
    ## [21] "VEGETACION_ARBUSTIVA"        "VEGETACION_DE_PARAMO"       
    ## [23] "CULTIVOS"                    "AGUA_NATURAL"               
    ## [25] "AGUA_ARTIFICIAL"             "VEGETACION_DE_HUMEDALES"    
    ## [27] "ESTRUCTURA_DE_VIVIENDA"      "INFRAESTRUCTURA"            
    ## [29] "CARRETERAS_Y_LOTES"          "VEGETACION_DE_ASENTAMIENTOS"
    ## [31] "SUELO_DESNUDO"               "NIEVE_HIELO"                
    ## [33] "OTRO"                        "NUBE_ININTELIGIBLE"

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
                                                    classfields = classCol)
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

    ## # A tibble: 6,000 x 38
    ##    PLOT_ID CENTER_LON CENTER_LAT SIZE_M SHAPE FLAGGED ANALYSES
    ##      <dbl>      <dbl>      <dbl>  <dbl> <chr> <lgl>      <dbl>
    ##  1       1      -79.1     -0.139     30 squa~ FALSE          0
    ##  2       2      -79.4     -2.71      30 squa~ FALSE          0
    ##  3       3      -79.4     -0.330     30 squa~ FALSE          0
    ##  4       4      -76.0     -0.822     30 squa~ FALSE          0
    ##  5       5      -79.9     -0.381     30 squa~ FALSE          0
    ##  6       6      -79.8     -3.55      30 squa~ FALSE          0
    ##  7       7      -79.7     -1.03      30 squa~ FALSE          0
    ##  8       8      -79.2     -1.35      30 squa~ FALSE          0
    ##  9       9      -77.2      0.132     30 squa~ FALSE          0
    ## 10      10      -79.1     -0.418     30 squa~ FALSE          0
    ## # ... with 5,990 more rows, and 31 more variables: SAMPLE_POINTS <dbl>,
    ## #   USER_ID <chr>, ANALYSIS_DURATION <lgl>, COLLECTION_TIME <dttm>,
    ## #   PL_LONGITUDE <dbl>, PL_LATITUDE <dbl>, PL_PLOTID <dbl>,
    ## #   PL_CLASS <dbl>, ARBOL_PRIMARIO <dbl>, ARBOL_SECUNDARIO <dbl>,
    ## #   ARBOL_DE_PLANTACION <dbl>, ARBOL_DE_MANGLE <dbl>,
    ## #   VEGETACION_HERBACEA_PASTOS <dbl>, VEGETACION_ARBUSTIVA <dbl>,
    ## #   VEGETACION_DE_PARAMO <dbl>, CULTIVOS <dbl>, AGUA_NATURAL <dbl>,
    ## #   AGUA_ARTIFICIAL <dbl>, VEGETACION_DE_HUMEDALES <dbl>,
    ## #   ESTRUCTURA_DE_VIVIENDA <dbl>, INFRAESTRUCTURA <dbl>,
    ## #   CARRETERAS_Y_LOTES <dbl>, VEGETACION_DE_ASENTAMIENTOS <dbl>,
    ## #   SUELO_DESNUDO <dbl>, NIEVE_HIELO <dbl>, OTRO <dbl>,
    ## #   NUBE_ININTELIGIBLE <dbl>, Primary <chr>, Secondary <chr>,
    ## #   LEVEL2 <chr>, LEVEL1 <chr>

The SEPAL stratified estimator tool works with integer classes. However, the data have been imported and prepared as characters. This section is for converting map values and validation values to integer values, and will export a csv file that can be uploaded into SEPAL.

``` r
#Code to convert these classes into factors, for use in building an error matrix.

# convert integers into text
finalTable <- convertToClasses(reclassed)

#strip out No_Data entries.
toRemove <- which(finalTable$LEVEL2 == "No_Data")
if (length(toRemove) > 0) {
    finalTable <- finalTable[-toRemove]
}

# Convert to factors. The levels need to be properly set. For the final numeric
# codes to match those of the map, they need to be in the same order as those
# of the map. 
refLevels <- c("Area_Sin_Cobertura_Vegetal", "Artificial", "Bosque", "Cultivo",
                             "Infraestructura", "Natural", "Paramos", "Vegetacion_Herbacea",
                             "Plantacion_Forestal", "Vegetacion_Arbustiva", 
                             "Area_Poblada", "Glaciar")

# Add the factors to the table
finalTable$reference <- factor(finalTable$LEVEL2, refLevels)
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
cleanAreas
```

    ##           areas areaClasses..1..
    ## 1  0.000000e+00                0
    ## 2  6.775595e+09                1
    ## 3  4.848717e+09                2
    ## 4  1.109274e+10                3
    ## 5  1.371502e+11                4
    ## 6  6.215711e+10                5
    ## 7  1.787481e+03                6
    ## 8  1.067806e+08                7
    ## 9  1.432138e+10                8
    ## 10 1.182804e+10                9
    ## 11 5.700854e+09               10
    ## 12 1.161763e+04               11
    ## 13 1.429933e+04               12
    ## 14 7.149121e+03               13
    ## 15 2.681210e+03               14
    ## 16 1.608619e+04               15

``` r
# Export table for upload to SEPAL
write_csv(cleanAreas, "data/area_rast.csv")
```

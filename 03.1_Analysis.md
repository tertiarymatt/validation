Accuracy Assessment and Area Estimation
================
MS Patterson, <tertiarymatt@gmail.com>
February 17, 2019

``` r
#cleanup workspace
rm(list = ls())

#setup
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.8
    ## v tidyr   0.8.2     v stringr 1.3.1
    ## v readr   1.2.1     v forcats 0.3.0

    ## -- Conflicts ------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
source("00.1_functions_en.R")
source("00.3_area_est_functions_en.R")
```

### Necessary data

**ceoTable** Outputs of all ceo projects, collated, and unifed with original point metadata. Produced in script `02_change_dataprep.R`.

**orig\_strata** Vector with numeric codes representing the original stratification of each sample.

**ref\_label** Vector with numeric codes representing the reference label for that year/map, for each sample.

**map\_label** Vector with numeric codes representing the map labels, for each sample.

**strata\_totals** Dataframe with two columns and number of rows equal to the total number of classes in the original strata. The first column must have the same codes found in the original stratification and the second must have the total number of PIXELS of each class in that original strata map.

**sample\_totals** Dataframe with two columns and number of rows equal to the total number of classes in the original strata. The first column must have the same codes found in the original stratification, and the second must have the total number of SAMPLE UNITS of each class collected from that original strata map.

**rfcodes** Vector with numeric values representing the reference codes present in ALL of the periods.
\#\#\#Importing Data
Importing the full data from the output file.

``` r
completeData <- read_csv("data/reference/complete/finalTable.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   PLOT_ID = col_double(),
    ##   CENTER_LON = col_double(),
    ##   CENTER_LAT = col_double(),
    ##   SIZE_M = col_double(),
    ##   FLAGGED = col_logical(),
    ##   ANALYSES = col_double(),
    ##   SAMPLE_POINTS = col_double(),
    ##   ANALYSIS_DURATION = col_logical(),
    ##   COLLECTION_TIME = col_datetime(format = ""),
    ##   PLOTID = col_double(),
    ##   STRATACLASS = col_double(),
    ##   MAPCLASS = col_double(),
    ##   changeL1 = col_logical(),
    ##   changeL2 = col_logical(),
    ##   refint = col_double(),
    ##   predint = col_double(),
    ##   strataint = col_double()
    ## )

    ## See spec(...) for full column specifications.

### Create **orig\_strata**

Vector with numeric codes representing the original stratification of each sample.

``` r
orig_strata <- completeData$strataint
```

### Create **ref\_label**

Vector with numeric codes representing the reference label for that year/map, for each sample.

``` r
ref_label <- completeData$refint
```

### Create **map\_label**

Vector with numeric codes representing the map labels, for each sample.

``` r
map_label <- completeData$predint
```

### Calculating sample\_totals

Generate a dataframe with two columns and number of rows equal to the total number of classes in the original strata. The first column must have the same codes found in the original stratification, and the second must have the total number of SAMPLE UNITS of each class collected from that original strata map.

``` r
sample_totals <- data.frame(table(completeData$strataint))
colnames(sample_totals) <- c("strata", "count")
```

### Set up rfcodes

Vector with numeric values representing the reference codes present in ALL of the periods.

### Setting rfcodes

These are the classes from the 2016 MAE map, plus mangroves and change classes derived from 2014/2016 map updating process.

``` r
# here as text, but used to convert to a factor and produce int codes.
rfcodes <- c("Area_Poblada", "Infraestructura", "Area_sin_Cobertura_Vegetal", 
                         "Glaciar", "Cuerpo_de_Agua_Natural", "Cuerpo_de_Agua_Artificial", 
                         "Bosque_Nativo", "Plantacion_Forestal", "Manglar", "Cultivo",  
                         "Paramos", "Vegetacion_Arbustiva", "Vegetacion_Herbacea", 
                         "FF", "GG","SS", "WW", "OO",
                         "FC", "FG", "FS", "FW", "CG", "CF", "CS", "GC", "GF", "GS", 
                         "WC", "WS", "OS", "CATCHALL")

rfcodesEN <- c("Settlement", "Infrastructure", "Barren", "Glacier", 
                             "Natural_Water", "Artificial_Water", "Primary_Forest", 
                             "Plantation_Forest", "Mangrove", "Secondary_Forest",
                             "Cropland", "Paramos", "Shrubland", "Herbland",
                             "FF", "GG","SS", "WW", "OO",
                             "FC", "FG", "FS", "FW", "CG", "CF", "CS", "GC", "GF", "GS",
                             "WC", "WS", "OS", "Catchall")
```

### Calculating strata\_totals

Import class data, reformat the feature properties to make a tidy export. Earth Engine exports a set of lists, with numbers formatted in a slightly odd manner. The lines below convert this into a clean set of areas and associated classes.

``` r
#set pixel size
pixel <- 30
#import area dta exported by GEE.
stableAreas <- read_csv("data/areas/Class_Areas.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   `system:index` = col_double(),
    ##   area = col_character(),
    ##   classes = col_character(),
    ##   .geo = col_logical()
    ## )

``` r
changeAreas <- read_csv("data/areas/Change_Class_Areas.csv")
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
stableClasses <- gsub("[", "", stableAreas$classes, fixed = TRUE) %>% 
    gsub("]", "", ., fixed = TRUE) %>% 
    str_split(., coll(", "), simplify = TRUE) %>% 
    gsub(" ", "_", ., fixed = TRUE)
stableClasses[1,]
```

    ##  [1] "Area_Poblada"               "Area_sin_Cobertura_Vegetal"
    ##  [3] "Bosque_Nativo"              "Cuerpo_de_Agua_Artificial" 
    ##  [5] "Cuerpo_de_Agua_Natural"     "Cultivo"                   
    ##  [7] "Glaciar"                    "Infraestructura"           
    ##  [9] "Manglar"                    "Paramos"                   
    ## [11] "Plantacion_Forestal"        "Vegetacion_Arbustiva"      
    ## [13] "Vegetacion_Herbacea"

``` r
changeClasses <- gsub("[", "", changeAreas$classes, fixed = TRUE) %>% 
    gsub("]", "", ., fixed = TRUE) %>% 
    str_split(., coll(", "), simplify = TRUE) %>% 
    gsub(" ", "_", ., fixed = TRUE)
changeClasses[1,]
```

    ##  [1] "CATCHALL" "CF"       "CG"       "CS"       "FC"       "FF"      
    ##  [7] "FG"       "FS"       "FW"       "GC"       "GF"       "GG"      
    ## [13] "GS"       "OO"       "OS"       "SS"       "WC"       "WS"      
    ## [19] "WW"

``` r
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
areaClasses <- factor(areaClasses, rfcodes)
class <- as.numeric(areaClasses)

cleanAreas <- data.frame(class, areas)
names(cleanAreas) <- c("class", "area")

#convert areas to pixel counts
strata_totals <- data.frame(class, areas/pixel^2)
names(strata_totals) <- c("strata", "pixelcount")
strata_totals
```

    ##    strata   pixelcount
    ## 1       1 2.451585e+06
    ## 2       3 8.368483e+05
    ## 3       7 1.374481e+08
    ## 4       6 1.579462e+06
    ## 5       5 2.579058e+06
    ## 6      10 9.510938e+07
    ## 7       4 6.965808e+04
    ## 8       2 2.342141e+05
    ## 9       9 8.280247e+05
    ## 10     11 1.671771e+07
    ## 11      8 1.198278e+06
    ## 12     12 7.968353e+06
    ## 13     13 8.750096e+05
    ## 14     32 1.325326e+06
    ## 15     24 1.702174e+06
    ## 16     23 6.296540e+05
    ## 17     25 8.919650e+04
    ## 18     19 2.878632e+06
    ## 19     14 7.569433e+03
    ## 20     20 8.597680e+04
    ## 21     21 1.390493e+04
    ## 22     22 4.716799e+04
    ## 23     26 1.369157e+06
    ## 24     27 2.255292e+05
    ## 25     15 8.685475e+06
    ## 26     28 1.307575e+04
    ## 27     18 1.619749e+04
    ## 28     31 3.305166e+03
    ## 29     16 3.463797e+03
    ## 30     29 3.159561e+05
    ## 31     30 7.626350e+03
    ## 32     17 1.120245e+04

### Calculate totarea\_pix

Total area of classes, in pixels

``` r
totarea_pix <- sum(strata_totals[,2])
totarea_pix
```

    ## [1] 285326321

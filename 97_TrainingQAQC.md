QA/QC for Validation Training Data
================
MS Patterson, <tertiarymatt@gmail.com>
January 02, 2019

### Required packages

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
library(irr)
```

    ## Loading required package: lpSolve

``` r
library(knitr)
source("00_functions.R")
```

### Import Data

``` r
kim <- read_csv("data/ceo-training-project-kim-plot-data-2019-01-02.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   SHAPE = col_character(),
    ##   FLAGGED = col_logical(),
    ##   USER_ID = col_character(),
    ##   ANALYSIS_DURATION = col_logical(),
    ##   COLLECTION_TIME = col_datetime(format = ""),
    ##   PL_NIVEL2 = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
steve <- read_csv("data/ceo-training-project-steve-plot-data-2019-01-02.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   SHAPE = col_character(),
    ##   FLAGGED = col_logical(),
    ##   USER_ID = col_character(),
    ##   ANALYSIS_DURATION = col_logical(),
    ##   COLLECTION_TIME = col_datetime(format = ""),
    ##   PL_NIVEL2 = col_character()
    ## )
    ## See spec(...) for full column specifications.

``` r
crossData <- rbind(kim, steve)
```

### Process data into form that can be used for irr

Provide overview of the process.

``` r
# Find and extract the class names. 
names(crossData)
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
    ## [16] "PL_NIVEL2"                             
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
classes <- colnames(crossData[17:35]) %>% 
    str_split(., coll(":"), simplify = TRUE) %>% 
    .[,2] %>% 
    gsub(" ", "_", .)

colnames(crossData)[17:35] <- classes
names(crossData)
```

    ##  [1] "PLOT_ID"                     "CENTER_LON"                 
    ##  [3] "CENTER_LAT"                  "SIZE_M"                     
    ##  [5] "SHAPE"                       "FLAGGED"                    
    ##  [7] "ANALYSES"                    "SAMPLE_POINTS"              
    ##  [9] "USER_ID"                     "ANALYSIS_DURATION"          
    ## [11] "COLLECTION_TIME"             "PL_LONGITUDE"               
    ## [13] "PL_LATITUDE"                 "PL_PLOTID"                  
    ## [15] "PL_FID_REFDTA"               "PL_NIVEL2"                  
    ## [17] "PRIMARY_TREE"                "SECONDARY_TREE"             
    ## [19] "PLANTATION_TREE"             "MANGROVE"                   
    ## [21] "HERBACEOUS/GRASS_VEGETATION" "SHRUB_VEGETATION"           
    ## [23] "PARAMO_VEGETATION"           "CROPS"                      
    ## [25] "NATURAL_WATER"               "ARTIFICIAL_WATER"           
    ## [27] "WETLAND_VEGETATION"          "HOUSING_STRUCTURE"          
    ## [29] "INFRASTRUCTURE"              "ROADS_AND_LOTS"             
    ## [31] "SETTLEMENT_VEGETATION"       "BARE_GROUND"                
    ## [33] "SNOW/ICE"                    "OTHER"                      
    ## [35] "CLOUDS/UNINTERPRETABLE"

``` r
# Process data into form that can be used for irr
# to do this, need to prepare a list of data frames with values for each class
# this is then fed to the irr functions. 
cross_tables <- rep(list(NA),length(classes))
names(cross_tables) <- classes

for (m in 1:length(cross_tables)) {
  cross_tables[[m]] <- select(crossData, "USER_ID", "PLOT_ID", classes[m]) %>%
    spread(., "USER_ID", classes[m]) %>%
    .[,-1] %>%
    na.omit(.) %>% 
    as.matrix(.)
}
```

### Calculate Metrics of Agreement

Describe metrics used, provide citations.
**Iota** is used to calculate overall agreement between two raters.

``` r
crossval_iota <- iota(cross_tables, scaledata = "q")
crossval_iota
```

    ##  iota for quantitative data (19 variables)
    ## 
    ##  Subjects = 158 
    ##    Raters = 2 
    ##      iota = 0.637

For checking agreement of individual classes, we can use several approaches.
The **intraclass correlcation coefficient** and **mean bivariate Pearson's** are two.

``` r
# Intraclass Correlation Coefficient
cross_icc <- list()
for (m in 1:length(cross_tables)) {
  cross_icc[[m]] <- icc(cross_tables[[m]], model = "oneway", type = "agreement")
}
names(cross_icc) <- classes

# make a "table" from data values in list
icc_values <- data_frame(length(classes), 5)
for (m in 1:length(cross_icc)) {
  icc_values[m,1] <- names(cross_icc[m])
  icc_values[m,2] <- round(cross_icc[[m]]$value, 4)
  icc_values[m,3] <- round(cross_icc[[m]]$lbound, 4)
  icc_values[m,4] <- round(cross_icc[[m]]$ubound, 4)
  icc_values[m,5] <- round(cross_icc[[m]]$p.value, 4)
}

colnames(icc_values) <- c("Class", "ICC", "Lower", "Upper", "Pvalue")

# Mean Bivariate Pearson's
cross_cor <- list()
for (m in 1:length(cross_tables)) {
  cross_cor[[m]] <- meancor(cross_tables[[m]])
}
```

    ## Warning in cor(ratings[, i], ratings[, j]): the standard deviation is zero

``` r
names(cross_cor) <- classes

# make a "table" from data values in list
cor_values <- data_frame(length(classes), 3)
for (m in 1:length(cross_icc)) {
    cor_values[m,1] <- names(cross_cor[m])
    cor_values[m,2] <- round(cross_cor[[m]]$value, 4)
    cor_values[m,3] <- round(cross_cor[[m]]$p.value, 4)
}
colnames(cor_values) <- c("Class", "Cor", "Pvalue")

# Assemble tables into one object for display. 
kable(bind_cols(icc_values, cor_values[,2:3]))
```

| Class                        |     ICC|    Lower|   Upper|  Pvalue|     Cor|  Pvalue1|
|:-----------------------------|-------:|--------:|-------:|-------:|-------:|--------:|
| PRIMARY\_TREE                |  0.6447|   0.5437|  0.7273|  0.0000|  0.6503|   0.0000|
| SECONDARY\_TREE              |  0.1374|  -0.0184|  0.2868|  0.0418|  0.1807|   0.0245|
| PLANTATION\_TREE             |  0.4921|   0.3645|  0.6015|  0.0000|  0.5109|   0.0000|
| MANGROVE                     |  0.9590|   0.9443|  0.9698|  0.0000|  0.9597|   0.0000|
| HERBACEOUS/GRASS\_VEGETATION |  0.7322|   0.6508|  0.7970|  0.0000|  0.7330|   0.0000|
| SHRUB\_VEGETATION            |  0.6958|   0.6059|  0.7682|  0.0000|  0.6965|   0.0000|
| PARAMO\_VEGETATION           |  0.2450|   0.0931|  0.3858|  0.0009|  0.2658|   0.0009|
| CROPS                        |  0.7137|   0.6279|  0.7824|  0.0000|  0.7203|   0.0000|
| NATURAL\_WATER               |  0.9874|   0.9827|  0.9907|  0.0000|  0.9888|   0.0000|
| ARTIFICIAL\_WATER            |  0.9692|   0.9581|  0.9774|  0.0000|  0.9695|   0.0000|
| WETLAND\_VEGETATION          |  0.8528|   0.8040|  0.8903|  0.0000|  0.8524|   0.0000|
| HOUSING\_STRUCTURE           |  0.8883|   0.8502|  0.9171|  0.0000|  0.9573|   0.0000|
| INFRASTRUCTURE               |  0.9528|   0.9359|  0.9653|  0.0000|  0.9559|   0.0000|
| ROADS\_AND\_LOTS             |  0.3809|   0.2397|  0.5065|  0.0000|  0.4140|   0.0000|
| SETTLEMENT\_VEGETATION       |  0.5946|   0.4838|  0.6866|  0.0000|  0.6752|   0.0000|
| BARE\_GROUND                 |  0.8636|   0.8180|  0.8985|  0.0000|  0.8712|   0.0000|
| SNOW/ICE                     |  0.9528|   0.9360|  0.9653|  0.0000|  0.9538|   0.0000|
| OTHER                        |     NaN|      NaN|     NaN|     NaN|      NA|       NA|
| CLOUDS/UNINTERPRETABLE       |  0.0409|  -0.1153|  0.1952|  0.3040|  0.2020|   0.0119|

### CEO Plot Table Reclassification

Use `addTopClasses()` to take a raw plot table produced by Collect Earth Online, and returns that table with a Primary and Secondary class field added.

``` r
# Find dominant landcover elements
crossData <- addTopClasses(crossData, plotfield = 1, flagfield = 6, 
                            classfields = c(17:35))
```

Now that we have the dominant landscape element classes, we can check for agreement between the interpreters on that, in this case just using simple percentages.

``` r
# make a little tibble with just the primary class, spread by rater
primaryAgree <- select(crossData, "USER_ID", "PLOT_ID", "Primary") %>%
    spread(., "USER_ID", "Primary") %>%
    na.omit(.)

# make a little tibble with just the secondary class, spread by rater
secondaryAgree <- select(crossData, "USER_ID", "PLOT_ID", "Secondary") %>%
    spread(., "USER_ID", "Secondary") %>%
    na.omit(.)

# Get raw percentage agreement on dominant class
round(sum(primaryAgree[,2] == primaryAgree[,3]) 
            / nrow(primaryAgree) * 100, 2)
```

    ## [1] 59.49

``` r
# Get raw precentage agreement on secondary class. 
round(sum(secondaryAgree[,2] == secondaryAgree[,3]) 
            / nrow(secondaryAgree) * 100, 2)
```

    ## [1] 47.47

QA/QC for Validation Data
================
MS Patterson, <tertiarymatt@gmail.com>
January 22, 2019

Set working directory to where data is being stored. + setwd

``` r
setwd("~/R/projects/validation")
```

### Required packages

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
library(irr)
```

    ## Loading required package: lpSolve

``` r
library(knitr)
source("00_functions.R")
```

### Import Data

Each worker or team should have the data they produced for the training data sets imported. There should be no need to process the data exported from CEO prior to working with it in this script.

``` r
# add a line for each worker or team. 
worker1 <- read_csv("data/ceo-training-project-3.1-plot-data-2019-01-07.csv")
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
worker2 <- read_csv("data/ceo-training-project-3.2-plot-data-2019-01-07.csv")
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

### Process data into form that can be used for irr

The imported data needs to be assembled and then cleaned up to make it easier to read and work with.

``` r
# make into one data frame. 
crossData <- bind_rows(worker1, worker2)

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
    ## [14] "PL_FID_REFDTA"                         
    ## [15] "PL_NIVEL2"                             
    ## [16] "PL_ORIG_FID"                           
    ## [17] "PL_PLOTID"                             
    ## [18] "LAND COVER:PRIMARY TREE"               
    ## [19] "LAND COVER:SECONDARY TREE"             
    ## [20] "LAND COVER:PLANTATION TREE"            
    ## [21] "LAND COVER:MANGROVE"                   
    ## [22] "LAND COVER:HERBACEOUS/GRASS VEGETATION"
    ## [23] "LAND COVER:SHRUB VEGETATION"           
    ## [24] "LAND COVER:PARAMO VEGETATION"          
    ## [25] "LAND COVER:CROPS"                      
    ## [26] "LAND COVER:NATURAL WATER"              
    ## [27] "LAND COVER:ARTIFICIAL WATER"           
    ## [28] "LAND COVER:WETLAND VEGETATION"         
    ## [29] "LAND COVER:HOUSING STRUCTURE"          
    ## [30] "LAND COVER:INFRASTRUCTURE"             
    ## [31] "LAND COVER:ROADS AND LOTS"             
    ## [32] "LAND COVER:SETTLEMENT VEGETATION"      
    ## [33] "LAND COVER:BARE GROUND"                
    ## [34] "LAND COVER:SNOW/ICE"                   
    ## [35] "LAND COVER:OTHER"                      
    ## [36] "LAND COVER:CLOUDS/UNINTERPRETABLE"

``` r
# create class column object to use in script.
# !!!If the structure of the data changes this MUST be updated!!!
classCol <- c(18:36)

classes <- colnames(crossData[classCol]) %>% 
    gsub(":", "_", .) %>% 
    gsub(" ", "_", .) %>% 
    gsub("/", "_", .)

# rename the columns in the data with the cleaned up version
colnames(crossData)[classCol] <- classes
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
    ## [14] "PL_FID_REFDTA"                         
    ## [15] "PL_NIVEL2"                             
    ## [16] "PL_ORIG_FID"                           
    ## [17] "PL_PLOTID"                             
    ## [18] "LAND_COVER_PRIMARY_TREE"               
    ## [19] "LAND_COVER_SECONDARY_TREE"             
    ## [20] "LAND_COVER_PLANTATION_TREE"            
    ## [21] "LAND_COVER_MANGROVE"                   
    ## [22] "LAND_COVER_HERBACEOUS_GRASS_VEGETATION"
    ## [23] "LAND_COVER_SHRUB_VEGETATION"           
    ## [24] "LAND_COVER_PARAMO_VEGETATION"          
    ## [25] "LAND_COVER_CROPS"                      
    ## [26] "LAND_COVER_NATURAL_WATER"              
    ## [27] "LAND_COVER_ARTIFICIAL_WATER"           
    ## [28] "LAND_COVER_WETLAND_VEGETATION"         
    ## [29] "LAND_COVER_HOUSING_STRUCTURE"          
    ## [30] "LAND_COVER_INFRASTRUCTURE"             
    ## [31] "LAND_COVER_ROADS_AND_LOTS"             
    ## [32] "LAND_COVER_SETTLEMENT_VEGETATION"      
    ## [33] "LAND_COVER_BARE_GROUND"                
    ## [34] "LAND_COVER_SNOW_ICE"                   
    ## [35] "LAND_COVER_OTHER"                      
    ## [36] "LAND_COVER_CLOUDS_UNINTERPRETABLE"

``` r
# Now that the data is cleaned up, it needs to be processed into a form that 
# can be used for by the `irr` package. To do this, need to prepare a list of 
# single class matrices with the PI values produced by each worker separated 
# into columns. 
```

``` r
# create an empty list to populate
cross_tables <- rep(list(NA),length(classes))
names(cross_tables) <- classes

# generate the matrices
for (m in 1:length(cross_tables)) {
    cross_tables[[m]] <- select(crossData, "USER_ID", "PLOT_ID", classes[m]) %>%
        spread(., "USER_ID", classes[m]) %>%
        .[,-1] %>%
        na.omit(.) %>% 
        as.matrix(.)
}
```

### Calculate Metrics of Agreement

**Iota** is used to calculate overall agreement between raters, and represents an overall look at how close they agree. A more granular approach is necessary to improve agreement, but iota provides a useful summary.

#### Citations

1.  Conger, A.J. (1980). Integration and generalisation of Kappas for multiple raters. Psychological Bulletin, 88, 322-328.
2.  Janson, H., & Olsson, U. (2001). A measure of agreement for interval or nominal multivariate observations. Educational and Psychological Measurement, 61, 277-289.

``` r
crossval_iota <- iota(cross_tables, scaledata = "q")
crossval_iota
```

    ##  iota for quantitative data (19 variables)
    ## 
    ##  Subjects = 55 
    ##    Raters = 2 
    ##      iota = 0.563

For checking agreement of individual classes, we can use several approaches.
The **intraclass correlation coefficient** and **mean bivariate Pearson's** are two. The ICC is used to measure consistency between two raters, and uses an F-test to test for significance.

#### Citations

1.  Bartko, J.J. (1966). The intraclass correlation coefficient as a measure of reliability. Psychological Reports, 19, 3-11.
2.  McGraw, K.O., & Wong, S.P. (1996), Forming inferences about some intraclass correlation coefficients. Psychological Methods, 1, 30-46.
3.  Shrout, P.E., & Fleiss, J.L. (1979), Intraclass correlation: uses in assessing rater reliability. Psychological Bulletin, 86, 420-428.

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

    ## Warning in cor(ratings[, i], ratings[, j]): the standard deviation is zero

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

| Class                                      |      ICC|    Lower|   Upper|  Pvalue|      Cor|  Pvalue1|
|:-------------------------------------------|--------:|--------:|-------:|-------:|--------:|--------:|
| LAND\_COVER\_PRIMARY\_TREE                 |   0.7742|   0.6426|  0.8615|  0.0000|   0.7731|   0.0000|
| LAND\_COVER\_SECONDARY\_TREE               |  -0.0239|  -0.2839|  0.2400|  0.5694|  -0.0442|   0.7500|
| LAND\_COVER\_PLANTATION\_TREE              |   0.9254|   0.8758|  0.9557|  0.0000|   0.9275|   0.0000|
| LAND\_COVER\_MANGROVE                      |   0.9273|   0.8788|  0.9568|  0.0000|   0.9294|   0.0000|
| LAND\_COVER\_HERBACEOUS\_GRASS\_VEGETATION |   0.5511|   0.3382|  0.7107|  0.0000|   0.6713|   0.0000|
| LAND\_COVER\_SHRUB\_VEGETATION             |   0.3643|   0.1134|  0.5720|  0.0027|   0.3712|   0.0074|
| LAND\_COVER\_PARAMO\_VEGETATION            |  -0.0123|  -0.2731|  0.2509|  0.5356|       NA|       NA|
| LAND\_COVER\_CROPS                         |   0.6371|   0.4505|  0.7706|  0.0000|   0.6324|   0.0000|
| LAND\_COVER\_NATURAL\_WATER                |   0.6588|   0.4798|  0.7854|  0.0000|   0.7758|   0.0000|
| LAND\_COVER\_ARTIFICIAL\_WATER             |   0.7017|   0.5389|  0.8141|  0.0000|   0.7402|   0.0000|
| LAND\_COVER\_WETLAND\_VEGETATION           |   0.1504|  -0.1159|  0.3971|  0.1328|   0.1440|   0.2991|
| LAND\_COVER\_HOUSING\_STRUCTURE            |   0.9726|   0.9537|  0.9839|  0.0000|   0.9756|   0.0000|
| LAND\_COVER\_INFRASTRUCTURE                |      NaN|      NaN|     NaN|     NaN|       NA|       NA|
| LAND\_COVER\_ROADS\_AND\_LOTS              |   0.9549|   0.9242|  0.9734|  0.0000|   0.9544|   0.0000|
| LAND\_COVER\_SETTLEMENT\_VEGETATION        |   0.5764|   0.3705|  0.7286|  0.0000|   0.6338|   0.0000|
| LAND\_COVER\_BARE\_GROUND                  |   0.8558|   0.7656|  0.9131|  0.0000|   0.8659|   0.0000|
| LAND\_COVER\_SNOW\_ICE                     |   1.0000|      NaN|     NaN|  0.0000|      NaN|      NaN|
| LAND\_COVER\_OTHER                         |      NaN|      NaN|     NaN|     NaN|       NA|       NA|
| LAND\_COVER\_CLOUDS\_UNINTERPRETABLE       |   0.2081|  -0.0567|  0.4461|  0.0608|   0.3926|   0.0046|

### CEO Plot Table Reclassification

Use `addTopClasses()` to take a raw plot table produced by Collect Earth Online, and return that table with a Primary and Secondary class field added.

``` r
# Find dominant landcover elements
crossData <- addTopClasses(crossData, plotfield = 1, flagfield = 6, 
                                                     classfields = c(classCol))
```

Now that we have the dominant landscape element classes, we can check for agreement between the interpreters on that, in this case just using simple percentages. We can simply here and use only simple agreement because this is a derived data set, and we are only interested in the implications of the level of agreement that we see between our workers.

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

    ## [1] 49.09

``` r
# Get raw percentage agreement on secondary class. 
round(sum(secondaryAgree[,2] == secondaryAgree[,3]) 
            / nrow(secondaryAgree) * 100, 2)
```

    ## [1] 30.91

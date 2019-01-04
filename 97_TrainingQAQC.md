QA/QC for Validation Training Data
================
MS Patterson, <tertiarymatt@gmail.com>
January 04, 2019

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
    gsub(" ", "_", .) %>% 
    gsub("/", "_", .)

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
    ## [21] "HERBACEOUS_GRASS_VEGETATION" "SHRUB_VEGETATION"           
    ## [23] "PARAMO_VEGETATION"           "CROPS"                      
    ## [25] "NATURAL_WATER"               "ARTIFICIAL_WATER"           
    ## [27] "WETLAND_VEGETATION"          "HOUSING_STRUCTURE"          
    ## [29] "INFRASTRUCTURE"              "ROADS_AND_LOTS"             
    ## [31] "SETTLEMENT_VEGETATION"       "BARE_GROUND"                
    ## [33] "SNOW_ICE"                    "OTHER"                      
    ## [35] "CLOUDS_UNINTERPRETABLE"

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
    ##  Subjects = 158 
    ##    Raters = 2 
    ##      iota = 0.637

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

| Class                         |     ICC|    Lower|   Upper|  Pvalue|     Cor|  Pvalue1|
|:------------------------------|-------:|--------:|-------:|-------:|-------:|--------:|
| PRIMARY\_TREE                 |  0.6447|   0.5437|  0.7273|  0.0000|  0.6503|   0.0000|
| SECONDARY\_TREE               |  0.1374|  -0.0184|  0.2868|  0.0418|  0.1807|   0.0245|
| PLANTATION\_TREE              |  0.4921|   0.3645|  0.6015|  0.0000|  0.5109|   0.0000|
| MANGROVE                      |  0.9590|   0.9443|  0.9698|  0.0000|  0.9597|   0.0000|
| HERBACEOUS\_GRASS\_VEGETATION |  0.7322|   0.6508|  0.7970|  0.0000|  0.7330|   0.0000|
| SHRUB\_VEGETATION             |  0.6958|   0.6059|  0.7682|  0.0000|  0.6965|   0.0000|
| PARAMO\_VEGETATION            |  0.2450|   0.0931|  0.3858|  0.0009|  0.2658|   0.0009|
| CROPS                         |  0.7137|   0.6279|  0.7824|  0.0000|  0.7203|   0.0000|
| NATURAL\_WATER                |  0.9874|   0.9827|  0.9907|  0.0000|  0.9888|   0.0000|
| ARTIFICIAL\_WATER             |  0.9692|   0.9581|  0.9774|  0.0000|  0.9695|   0.0000|
| WETLAND\_VEGETATION           |  0.8528|   0.8040|  0.8903|  0.0000|  0.8524|   0.0000|
| HOUSING\_STRUCTURE            |  0.8883|   0.8502|  0.9171|  0.0000|  0.9573|   0.0000|
| INFRASTRUCTURE                |  0.9528|   0.9359|  0.9653|  0.0000|  0.9559|   0.0000|
| ROADS\_AND\_LOTS              |  0.3809|   0.2397|  0.5065|  0.0000|  0.4140|   0.0000|
| SETTLEMENT\_VEGETATION        |  0.5946|   0.4838|  0.6866|  0.0000|  0.6752|   0.0000|
| BARE\_GROUND                  |  0.8636|   0.8180|  0.8985|  0.0000|  0.8712|   0.0000|
| SNOW\_ICE                     |  0.9528|   0.9360|  0.9653|  0.0000|  0.9538|   0.0000|
| OTHER                         |     NaN|      NaN|     NaN|     NaN|      NA|       NA|
| CLOUDS\_UNINTERPRETABLE       |  0.0409|  -0.1153|  0.1952|  0.3040|  0.2020|   0.0119|

Reviewing the results of this table and the actual plot classifications, one can see that the bulk of the disagreements occurred between Primary and Secondary Forest, Shrub and Forest, and Shrub and Herbaceous cover. Given the imagery and location, this isn't terribly surprising.
Minor disagreements occurred around Crops vs. Herbaceous, Barren vs. Herbaceous, and within the Settlement cluster of classes.
Note that most of the differences in agreement are occuring *within* level 1 classes, and not across them.

### CEO Plot Table Reclassification

Use `addTopClasses()` to take a raw plot table produced by Collect Earth Online, and return that table with a Primary and Secondary class field added.

``` r
# Find dominant landcover elements
crossData2 <- addTopClasses(crossData, plotfield = 1, flagfield = 6, 
                            classfields = c(17:35))
```

Now that we have the dominant landscape element classes, we can check for agreement between the interpreters on that, in this case just using simple percentages.

``` r
# make a little tibble with just the primary class, spread by rater
primaryAgree <- select(crossData2, "USER_ID", "PLOT_ID", "Primary") %>%
    spread(., "USER_ID", "Primary") %>%
    na.omit(.)

# make a little tibble with just the secondary class, spread by rater
secondaryAgree <- select(crossData2, "USER_ID", "PLOT_ID", "Secondary") %>%
    spread(., "USER_ID", "Secondary") %>%
    na.omit(.)

# Get raw percentage agreement on dominant class
round(sum(primaryAgree[,2] == primaryAgree[,3]) 
            / nrow(primaryAgree) * 100, 2)
```

    ## [1] 59.49

``` r
# Get raw percentage agreement on secondary class. 
round(sum(secondaryAgree[,2] == secondaryAgree[,3]) 
            / nrow(secondaryAgree) * 100, 2)
```

    ## [1] 47.47

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

``` r
# Adding the Level 2 Classes. 
reclassed <- crossData2 %>% 
    mutate(
        LEVEL2 = case_when(
            PRIMARY_TREE >= 30 ~ "Primary_Forest",
            SECONDARY_TREE >= 30 ~ "Secondary_Forest",
            PLANTATION_TREE >= 30 ~ "Plantation_Forest",
            MANGROVE >= 30 ~ "Mangrove",
            HERBACEOUS_GRASS_VEGETATION >= 30 ~ "Herbland",
            SHRUB_VEGETATION >= 30 ~ "Shrubland",
            PARAMO_VEGETATION > 0 ~ "Paramo",
            CROPS >= 50 ~ "Cropland",
            NATURAL_WATER + 
                WETLAND_VEGETATION >= 50 ~  "Natural_Water",
            ARTIFICIAL_WATER + 
                WETLAND_VEGETATION >= 50 ~  "Artificial_Water",
            WETLAND_VEGETATION >= 50 & 
                ARTIFICIAL_WATER > 0 ~ "Artificial_Water",
            WETLAND_VEGETATION >= 50 ~ "Natural_Water",
            HOUSING_STRUCTURE + 
                SETTLEMENT_VEGETATION + 
                ROADS_AND_LOTS >= 30 ~ "Settlement",
            ROADS_AND_LOTS >= 30 & 
                HOUSING_STRUCTURE > 0 ~ "Settlement",
            INFRASTRUCTURE + 
                SETTLEMENT_VEGETATION + 
                ROADS_AND_LOTS >= 30 ~ "Infrastructure",
            ROADS_AND_LOTS >= 30 ~ "Infrastructure",
            BARE_GROUND >= 70 ~ "Non-vegetated",
            BARE_GROUND +
                HOUSING_STRUCTURE +
                SETTLEMENT_VEGETATION >= 30 ~ "Settlement",
            BARE_GROUND +
                INFRASTRUCTURE +
                SETTLEMENT_VEGETATION >= 30 ~ "Infrastructure",
            BARE_GROUND +
                ROADS_AND_LOTS >= 30 ~ "Infrastructure",
            SNOW_ICE +
                BARE_GROUND >= 70 ~ "Glacial",
            OTHER >= 50 ~ "Other",
            CLOUDS_UNINTERPRETABLE >= 50 ~ "No_Data",
            Primary == "FLAGGED" ~ "No_Data",
            TRUE ~ "Mosaic"
        )
    )
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
reclassed <- reclassed %>% 
    mutate(
        LEVEL1 = case_when(
            LEVEL2 == "Primary_Forest" |
                LEVEL2 == "Secondary_Forest" |
                LEVEL2 == "Plantation_Forest" |
                LEVEL2 == "Mangrove" ~ "Forest_Lands",
            LEVEL2 == "Herbland" | 
                LEVEL2 == "Shrubland" | 
                LEVEL2 == "Paramo" ~ "Grasslands",
            LEVEL2 == "Cropland" ~ "Croplands",
            LEVEL2 == "Natural_Water" |
                LEVEL2 == "Artificial_Water" ~ "Wetlands",
            LEVEL2 == "Settlement" |
                LEVEL2 == "Infrastructure" ~ "Settlements",
            LEVEL2 == "Glacial" |
                LEVEL2 == "Non-vegetated" |
                LEVEL2 == "Other" |
                LEVEL2 == "Mosaic" ~ "Other_Lands",
            LEVEL2 == "No_Data" ~ "No_Data"
        )
    )
```

Now that we have the LULC classes assigned, we can check for agreement between the interpreters on that, in this case just using simple percentages.

``` r
# make a little tibble with just the level 2 class, spread by rater
lvl2Agree <- select(reclassed, "USER_ID", "PLOT_ID", "LEVEL2") %>%
    spread(., "USER_ID", "LEVEL2") %>%
    na.omit(.)

# make a little tibble with just the level 1 class, spread by rater
lvl1Agree <- select(reclassed, "USER_ID", "PLOT_ID", "LEVEL1") %>%
    spread(., "USER_ID", "LEVEL1") %>%
    na.omit(.)

# Get raw percentage agreement on level 2 class
round(sum(lvl2Agree[,2] == lvl2Agree[,3]) 
            / nrow(lvl2Agree) * 100, 2)
```

    ## [1] 75.32

``` r
# Get raw percentage agreement on level 1 class. 
round(sum(lvl1Agree[,2] == lvl1Agree[,3]) 
            / nrow(lvl1Agree) * 100, 2)
```

    ## [1] 85.44

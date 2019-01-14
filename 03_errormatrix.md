Error Matrix Generation
================
MS Patterson, <tertiarymatt@gmail.com>
January 14, 2019

### Required packages

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
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
source("00_functions.R")
```

Task list for this script:
1. DONE. Bring in validation data. Need to have map values attached.
1. DONE. Convert map values and validation values to factors.
1. DONE. Ensure factors have identical levels.
1. DONE. Construct confusion matrix.
1. Convert to area-based error matrix.
1. Calculate standard errors of area estimates. 1. Calculate error metrics.

### Data Prep

Import the validation data. Either properly configure and source 02\_dataprep.R or run it manually.

``` r
# Import the collected validaiton data and map values. 
source("02_dataprep.R")
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

The error matrix tool works with factors. The data have been imported and prepared as characters. Convert map values and validation values to factors.

``` r
#Code to convert these classes into factors, for use in building an error matrix.

# convert integers into text
finalTable <- convertToClasses(reclassed)

#strip out No_Data entries.
toRemove <- which(finalTable$LEVEL2 == "No_Data")
finalTable <- finalTable[-toRemove,]

# Convert to factors.

refLevels <- c("Primary_Forest", "Secondary_Forest", "Plantation_Forest", 
                             "Mangrove", "Cropland", "Natural_Water",  "Artificial_Water",
                             "Paramo", "Shrubland", "Herbland", "Settlement", "Infrastructure", 
                             "Glacier", "Non-vegetated")

reference <- factor(finalTable$LEVEL2, refLevels)
map <- factor(finalTable$MapClass, refLevels)
```

### A Basic Confusion Matrix

Construct the confusion matrix, using the extracted vectors of values. We can use the `caret` package for this process.

``` r
errorMatrix <- confusionMatrix(map, reference, positive = NULL, 
                                                            dnn = c("Prediction", "Reference"))
```

### Create an area-based data frame.

``` r
# Extract the error matrix, and make into a data frame for easier addressing.
# Remember that reference are columns, and predicted map values are rows!
eM <- data.frame(unclass(errorMatrix$table))
```

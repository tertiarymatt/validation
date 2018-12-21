Determing Sample Size
================
MS Patterson, <tertiarymatt@gmail.com>
December 20, 2018

### Required packages

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 2.2.1     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.5
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ----------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
source("00_functions.R")
```

### This section is used to determing sample size needed for the project.

``` r
overallN <- genSample1(p0 = 0.75, h = 0.01, alpha = 0.05)
overallN
```

    ## [1] 7203

``` r
deforestationN <- genSample1(p0 = 0.90, h = 0.01, alpha = 0.05)
deforestationN
```

    ## [1] 3457

``` r
averageN <- mean(c(overallN, deforestationN))
averageN
```

    ## [1] 5330

N needs to be increased a bit to cover the possibility of bad imagery, etc.

``` r
actualN <- 5500
```

Use the optimization tool from Wagner & Stehman, 2015 to split samples into two pools, stable and change. The first step is to create an area-based error matrix, with estimated proportions.

We will assume a larger than realistic change to ensure sufficient power.

``` r
error <- matrix(nrow = 2, ncol = 2, 
                c(0.05, 0.0125, 
                  0.0125, 0.925))

# Check that error matrix is correctly made. 
checkErrorMatrix(error)
```

    ## [1] "Error matrix appears correctly formed."

Next we will optimize the split of the sample pool.

``` r
samplePool <- optimizeSplit(error, actualN, c("Change", "Stable"))
samplePool
```

    ## Change Stable 
    ##   1256   4244

The next step requires the use of Google Earth Engine to assign the split samples to their respective classes, and generating a point file. R could be used for this, but this would require that the map be downloaded and imported. The Earth Engine script is included below.

> // Stack class map and latitude/longitude
> var sampleImage = ee.Image.cat(toSample, ee.Image.pixelLonLat());
>
> // Sample within the study area and the loss mask
> var sample = sampleImage.stratifiedSample({
> numPoints: 50,
> classBand: 'class',
> region: studyArea,
> scale: 30,
> seed: 17,
> });
>
> // Add back in geometry and visualize
> var sample\_geo\_assembly = sample.map(function(point){
> var long = point.get('longitude');
> var lat = point.get('latitude');
> var geom = ee.Algorithms.GeometryConstructors.Point(\[long, lat\]);
> return point.setGeometry(geom);
> });
>
> Map.addLayer(sample\_geo\_assembly.draw('0000FF',2),{},'Sample locations');
> Map.centerObject(sample\_geo\_assembly, 9);
>
> print('Point Sample Data', sample\_geo\_assembly);
>
> Export.table.toDrive({
> collection: sample\_geo\_assembly,
> description:'Validation\_Sample',
> folder:'driveFolderHere',
> fileFormat: "CSV"
> });

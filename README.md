# validation
A collection of scripts used for validation/accuracy assessment of land cover products.

## Basic Workflow

### Generate Samples
To generate samples, a combination of Google Earth Engine (GEE) and R scripts are used. The [first GEE script](https://code.earthengine.google.com/5c56f4448e391a3d79e58a3c94d7fbfe) estimates the area of change in the study area. This is used to inform the first R script (01_samplesize.R) to optimize the split between the stable and change classes. 

After a sample size has been generated, the [second GEE script](https://code.earthengine.google.com/c80b40479080b72ea12a59c13e0cdeea) and [third GEE scripts](https://code.earthengine.google.com/a16be02f16cd1c167d9afff70cc89064) are used to generate the points for the stable and change classes, respectively. These points are exported to two CSV files. Class areas are also exported, which are essential for performing validation when using a map with strata that do not match the original stratification map. 

### PI Work
The two CSV files are then concatenated, and ordered such that they can be imported into Collect Earth Online (CEO); the field order (and names) must be Longtidue, Latitude, and Plotid. Projects are constructed in CEO and assigned, and Photo Interpretation performed to collect data. 

Once the PI work is complete, the resulting tables are exported from CEO and brought together into one table. This table is re-imported into GEE as an asset, and the map values from the data to be validated are then added to the table, and it is re-exported, in [GEE script 4](https://code.earthengine.google.com/095b34fe3226cc32553fe4fdf3824a73). 

### Validation
Once the full table has been assembled, the script 02_change_dataprep.R or 02_dataprep.R are used to clean and recode the data for analysis in R, as well as to process the area tables. 

The analysis R scripts (all script beginning with a *03*) are then used to analyze the prepared data and produce estimates of overall, user's, and producer's accuracy. Estimates of class area are all produced with uncertainties. 
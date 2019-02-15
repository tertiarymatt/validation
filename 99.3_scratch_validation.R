#' ---
#' title: "Scratch File for Development"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#setup
library(tidyverse)
source("00.1_functions_en.R")
source("00.3_area_est_functions_en.R")

#' ####TODO  
#' 0. [ ] **Collate CEO outputs:** Bring together all samples, unify with original 
#' data derived from GEE/MAE to get stratifying map classes and old validation
#' back into the file. Should be able to join on Lat/Long. 
#' 0. [ ] **Process CEO outputs:** run through data prep scripts to get ref classes.
#' Will likely require updating the functions, again. Might have to wait on 
#' this step for the actual maps to be output, to avoid another reclass on the 
#' data. Potential chokepoint.  
#' 1. [ ] **orig_strata:** More or less done when the collation is complete,
#' can be pulled out of that data.  
#' 2. [ ] **ref_label:**  Pull out of processed CEO data. Can be completed prior
#' to having final maps, but is potential chokepoint linked to processing.    
#' 3. [ ] **map_label:**  Data goes back into GEE to sample final maps.
#' Obviously constrained by existence of final maps.    
#' 4. [x] **strata_totals:** exported with samples, needs to be processed and
#'  converted into pixel counts. Trivial.  
#' 5. [ ] **sample_totals:** Derived from a summary of the collatted CEO data.      
#' 6. [x] **rfcodes:**  Easy-peasy lemon squeezy. Reclass possible, though.  
#' 7. [x] **pixel**  = 30.  
#' 8. [x] **totarea_pix:** Easy to get from total areas.  
#' 9. [ ] **Analyze:** Use new functions to rebalance sample and calculate 
#' accuracies. Rejoice, maybe!  
#' 10. [ ] **DREAD:** Reclass to improve accuracy and re-run?  
#' 11. [ ] **Report:** Outline shortly, then simply fill in. 
#' 
#' ### [ ] **Collate CEO outputs**

#+ Import and Prepare Data
#Import original samples
stable <- read_csv("data/points/Validation_Sample_for_CEO.csv")
change <- read_csv("data/points/Validation_Change_Sample_for_CEO.csv")

allPoints <- bind_rows(stable, change)

# Import gathered CEO data, prepare
ceoTable <- read_csv("data/reference/ceo-pi_ecuador_03.2-plot-data-2019-02-11.csv")
colnames(ceoTable)

#create metadata data table. Use the colnames to find them and adjust columns.
metadata <- ceoTable[,1:11]
head(metadata)

metadata <- left_join(metadata, allPoints, by = c("CENTER_LON" = "LONGITUDE", 
																			"CENTER_LAT" = "LATITUDE"))

# Can add this section in to 02_change_dataprep.R when time comes

#' ---
#' title: "QA/QC for Validation Data"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' ### Required packages
#+ Packages
library(tidyverse)
library(irr)
library(stargazer)
source("00_functions.R")

#' ### Import Data
#+ Import



#' ### Process data into form that can be used for irr  
#' Provide overview of the process. 

#+ Process



#' ### Calculate Metrics of Agreement  
#' Describe metrics used, provide citations. 

#+ IRR Metrics




#' ### Material below this line is imported from another project, as examples.
#' 
#+  Old Code
#' Import Data
cross1 <- read_csv("data/ceo-assembly-cross-validation-5groups-result.csv")

# Process data into form that can be used for irr
classes <- colnames(cross1)[11:30]

cross_tables <- rep(list(NA),length(classes))

names(cross_tables) <- classes

for(m in 1:length(cross_tables)) { 
  cross_tables[[m]] <- select(cross1, "Group", "PLOT_ID", classes[m]) %>% 
    spread(., "Group", classes[m]) %>% 
    .[,-1] %>%
    as.matrix(.)
}

# Calculate metrics of agreement ----

# Iota, a multivariate metric of agreement
crossval_iota <- iota(cross_tables, scaledata = "q")

# Intraclass Correlation Coefficient
cross_icc <- list()
for(m in 1:length(cross_tables)) { 
  cross_icc[[m]] <- icc(cross_tables[[m]], model="oneway", type="agreement")
}
names(cross_icc) <- classes


# make a "table" 
icc_values <- matrix(NA, 20, 2)
for(m in 1:length(cross_icc)) { 
  icc_values[m,1] <- names(cross_icc[m])
  icc_values[m,2] <- cross_icc[[m]]$value
}


# Krippendorf's alpha
cross_ka <- list()
for(m in 1:length(cross_tables)) { 
  cross_ka[[m]] <- kripp.alpha(t(cross_tables[[m]]), method="ratio")
}
names(cross_ka) <- classes

#Mean Bivariate Pearson's
cross_cor <- list()
for(m in 1:length(cross_tables)) { 
  cross_cor[[m]] <- meancor(cross_tables[[m]])
}
names(cross_cor) <- classes


#### making pretty tables for output to paper ----
icc_comp[,2:4] <- round(icc_comp[,2:4], 4)

stargazer(icc_comp, 
          title = "Changes in intraclass correlation coefficient (ICC) between training sessions. 
          Dashes indicate classes with too few non-zero values to calculate.", summary = F, 
          colnames = T, column.labels = c("Class Names","ICC Session 1", "ICC Session 2", "Change"), rownames = F)

#### Concordance on Reclassed Data ----

newCross01 <- cross1 %>% 
  transmute(., 
            PlotID = PLOT_ID,
            Group = Group,
            Flagged = FLAGGED,
            Forest = FOREST_TREE, 
            Mangrove = MANGROVE,
            Grass = GRASS,
            Shrub = SHRUB_W_WOOD,
            Crop = CROP_PLANTATION_ORCHARD + CROP,
            Rice = PADDY_RICE,
            Impervious = IMPERVIOUS_BUILT_SURFACE,
            BuiltVeg = BUILT_TREE + BUILT_VEGETATION,
            Wetlands = WETLANDS_MUDFLAT_TIDALFLAT + AQUATIC_VEGETATION_FLOODED_FOREST,
            Water = WATER + AQUACULTURE_POND,
            Snow = SNOW_ICE,
            Barren = BARREN_SAND_DUNE + ROCKY_MOUNTAIN + MINING,
            Other = OTHER + UNKNOWN)


# Process data into form that can be used for irr
classes02 <- colnames(newCross01)[04:16]

cross01_tables <- rep(list(NA),length(classes02))

names(cross01_tables) <- classes02


for(m in 1:length(cross01_tables)) { 
  cross01_tables[[m]] <- select(newCross01, "Group", "PlotID", classes02[m]) %>% 
    spread(., "Group", classes02[m]) %>% 
    .[,-1] %>%
    as.matrix(.)
}


### Calculate metrics of agreement for second classification scheme----

# Iota, a multivariate metric of agreement
newCross01_iota <- iota(cross01_tables, scaledata = "q")

# Intraclass Correlation Coefficient
newCross01_icc <- list()
for(m in 1:length(cross01_tables)) { 
  newCross01_icc[[m]] <- icc(cross01_tables[[m]], model="oneway", type="agreement")
}
names(newCross01_icc) <- classes02


# make a "table" 
newIcc_values <- matrix(NA, 13, 2)
for(m in 1:length(newCross01_icc)) { 
  newIcc_values[m,1] <- names(newCross01_icc[m])
  newIcc_values[m,2] <- newCross01_icc[[m]]$value
}


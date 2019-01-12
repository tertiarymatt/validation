#' ---
#' title: "QA/QC for Validation Training Data"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' ### Required packages
#+ Packages
library(tidyverse)
library(irr)
library(knitr)
source("00_functions.R")

#' ### Import Data
#+ Import

steve2 <- read_csv("data/ceo-training-project-3.1-plot-data-2019-01-07.csv")
kim2 <- read_csv("data/ceo-training-project-3.2-plot-data-2019-01-07.csv")

crossData2 <- rbind(kim2, steve2)

# remove "flagged plots" entries  
flagged <- which(crossData2$FLAGGED == "TRUE")
crossData2 <- crossData2[-flagged,]


#' ### Process data into form that can be used for irr  
#' Provide overview of the process. 

#+ Process2
# Find and extract the class names. 
names(crossData2)
classes <- colnames(crossData2[18:36]) %>% 
	str_split(., coll(":"), simplify = TRUE) %>% 
	.[,2] %>% 
	gsub(" ", "_", .) %>% 
	gsub("/", "_", .)

colnames(crossData2)[18:36] <- classes
names(crossData2)

# Process data into form that can be used for irr
# to do this, need to prepare a list of data frames with values for each class
# this is then fed to the irr functions. 
cross_tables <- rep(list(NA),length(classes))
names(cross_tables) <- classes

for (m in 1:length(cross_tables)) {
	cross_tables[[m]] <- select(crossData2, "USER_ID", "PLOT_ID", classes[m]) %>%
		spread(., "USER_ID", classes[m]) %>%
		.[,-1] %>%
		na.omit(.) %>% 
		as.matrix(.)
}

#' ### Calculate Metrics of Agreement  
#' **Iota** is used to calculate overall agreement between raters, and represents
#' an overall look at how close they agree. A more granular approach is necessary
#' to improve agreement, but iota provides a useful summary.  
#' 
#' #### Citations  
#' 1. Conger, A.J. (1980). Integration and generalisation of Kappas for multiple
#'  raters. Psychological Bulletin, 88, 322-328.  
#' 1. Janson, H., & Olsson, U. (2001). A measure of agreement for interval or
#'  nominal multivariate observations. Educational and Psychological 
#'  Measurement, 61, 277-289. 

#+ Iota, a multivariate metric of agreement2
crossval_iota <- iota(cross_tables, scaledata = "q")
crossval_iota

#' For checking agreement of individual classes, we can use several approaches.  
#' The **intraclass correlation coefficient** and 
#' **mean bivariate Pearson's** are two.
#' The ICC is used to measure consistency between two raters, and uses an F-test
#' to test for significance.  
#'  
#' #### Citations  
#'  1. Bartko, J.J. (1966). The intraclass correlation coefficient as a measure of 
#'  reliability. Psychological Reports, 19, 3-11.  
#'  1. McGraw, K.O., & Wong, S.P. (1996), Forming inferences about some intraclass 
#'  correlation coefficients. Psychological Methods, 1, 30-46.  
#'  1. Shrout, P.E., & Fleiss, J.L. (1979), Intraclass correlation: uses in 
#'  assessing rater reliability. Psychological Bulletin, 86, 420-428.  

#+ Per-class agreement2

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

#' Reviewing the results of this table and the actual plot classifications, 
#' one can see that the bulk of the disagreements occurred between 
#' Primary and Secondary Forest, Shrub and Forest, and Shrub and Herbaceous 
#' cover. Given the imagery and location, this isn't terribly surprising.  
#' Minor disagreements occurred around Crops vs. Herbaceous, 
#' Barren vs. Herbaceous, and within the Settlement cluster of classes.  
#' Note that most of the differences in agreement are occuring _within_ level 
#' 1 classes, and not across them.   
#' 
#' ### CEO Plot Table Reclassification
#'
#' Use `addTopClasses()` to take a raw plot table produced by Collect Earth
#' Online, and return that table with a Primary and Secondary class field
#' added.

#+ Find dominant landcover elements2
# Find dominant landcover elements
crossData2 <- addTopClasses(crossData2, plotfield = 1, flagfield = 6, 
														classfields = c(18:36))

#' Now that we have the dominant landscape element classes, we can check for
#'  agreement between the interpreters on that, in this case just using simple
#'  percentages.

#+ TopClassAgreement2
# make a little tibble with just the primary class, spread by rater
primaryAgree2 <- select(crossData2, "USER_ID", "PLOT_ID", "Primary") %>%
	spread(., "USER_ID", "Primary") %>%
	na.omit(.)

# make a little tibble with just the secondary class, spread by rater
secondaryAgree2 <- select(crossData2, "USER_ID", "PLOT_ID", "Secondary") %>%
	spread(., "USER_ID", "Secondary") %>%
	na.omit(.)

# Get raw percentage agreement on dominant class
round(sum(primaryAgree2[,2] == primaryAgree2[,3]) 
			/ nrow(primaryAgree2) * 100, 2)

# Get raw percentage agreement on secondary class. 
round(sum(secondaryAgree2[,2] == secondaryAgree2[,3]) 
			/ nrow(secondaryAgree2) * 100, 2)

#' ### Level 1 and Level 2 LULC classes  
#' Next steps: add code to convert to level 1 and level 2 classes, and test 
#' agreement/consistency at that level. Because the data is collected a finer
#' level of detail than either Level 1 or Level 2, Level 2 is produced first.  
#'
#' #### Level 2 LULC Thresholds:  
#' Primary Forest = Secondary tree >= 30%  
#' Secondary Forest = Secondary tree >= 30%  
#' Plantation = Plantation tree >= 30%  
#' Mangrove = Mangrove >= 30%  
#' Grass/herbland = Herbaceous veg > 0% & Tree < 30% & Shrub < 30%  
#' Shrubland = Shrub vegetation >= 30%, Tree < 30%  
#' Paramo = Paramo > 0%  
#' Cropland = Crops >= 50%  
#' Water = Natural water >= 50% | Wetland vegetation >= 50%  
#' Settlement = Houses & Commercial >= 30% | Urban vegetation >= 30%  
#' Infrastructure = Roads and Lots >= 30% | Infrastructure building >= 30%  
#' Non-vegetated = barren >= 70%  
#' Glacial = Snow/Ice >= 70%  

#+ Level2Classes2
# Adding the Level 2 Classes. 
reclassed2 <- crossData2 %>% 
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

#' #### Level 1 LULC Conversions:
#' Forest Lands = Primary, Secondary, Plantation, Mangrove  
#' Grasslands = Herbland, Shrubland, Paramo  
#' Croplands = Cropland  
#' Wetlands = Aritifical Water, Natural Water  
#' Settlements = Settlement, Infrastructure  
#' Other Lands = Glacial, Non-vegetated, Other, Mosaic  
#' No Data = No Data  

# Adding the Level one classes.
reclassed2_2 <- reclassed2 %>% 
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


#' Now that we have the LULC classes assigned, we can check for agreement 
#' between the interpreters on that, in this case just using simple percentages.

#+ LULCAgreement2
# make a little tibble with just the level 2 class, spread by rater
lvl2Agree2 <- select(reclassed2_2, "USER_ID", "PLOT_ID", "LEVEL2") %>%
	spread(., "USER_ID", "LEVEL2") %>%
	na.omit(.)

# make a little tibble with just the level 1 class, spread by rater
lvl1Agree2 <- select(reclassed2_2, "USER_ID", "PLOT_ID", "LEVEL1") %>%
	spread(., "USER_ID", "LEVEL1") %>%
	na.omit(.)

# Get raw percentage agreement on level 2 class
round(sum(lvl2Agree2[,2] == lvl2Agree2[,3]) 
			/ nrow(lvl2Agree2) * 100, 2)

# Get raw percentage agreement on level 1 class. 
round(sum(lvl1Agree2[,2] == lvl1Agree2[,3]) 
			/ nrow(lvl1Agree2) * 100, 2)

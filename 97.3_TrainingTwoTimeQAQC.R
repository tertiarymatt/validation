#' ---
#' title: "QA/QC for Validation Training Data"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' Set working directory to where data is being stored.
#'+ setwd
setwd("~/R/projects/validation")

#' ### Required packages
#+ Packages
library(tidyverse)
library(irr)
library(knitr)
source("00_functions.R")

#' ### Import Data
#+ Import
kim <- read_csv("data/ceo-two-time-training-1.2-plot-data-2019-01-23.csv")
steve <- read_csv("data/ceo-two-time-training-1.1-plot-data-2019-01-23.csv")

crossData <- rbind(kim, steve)

#' ### Process data into form that can be used for irr  
#' Provide overview of the process. 

#time one plot data
time1 <- crossData[,16:34]

#time two plot data
time2 <- crossData[,35:53]

#create metadata data table. Use the colnames to find them and adjust columns.
metadata <- crossData[,1:15]

#add metadata to the time tables.
time1 <- bind_cols(metadata, time1)
time2 <-  bind_cols(metadata, time2)

# class names need to be pulled and cleaned up.
colnames(time1)

# create class column object to use in script.
# If the structure of the data changes this MUST be updated.
classCol <- c(16:34)

classes <- colnames(time1[classCol]) %>% 
	str_split(., coll(":"), simplify = TRUE) %>% 
	.[,2] %>% 
	gsub(" ", "_", .) %>% 
	gsub("/", "_", .)

colnames(time1)[classCol] <- classes
colnames(time2)[classCol] <- classes

#verify the tables have the same names
colnames(time1) == colnames(time2)

#' ### CEO Point Table Reclassification
#'
#' Use `addTopClasses()`` to take a raw plot table produced by Collect Earth
#' Online, and returns that table with a Primary and Secondary class field
#' added.

#+ Find dominant landcover elements
time1 <- addTopClasses(time1, plotfield = 1, flagfield = 6, 
											 classfields = classCol)

time2 <- addTopClasses(time2, plotfield = 1, flagfield = 6, 
											 classfields = classCol)

#' Then use primary and/or secondary classes and threshold values to convert to
#' end classification.
#' 
#' ### Level 1 and Level 2 LULC classes
#' Next steps: add code to convert to level 1 and level 2 classes, and test 
#' agreement/consistency at that level. Because the data is collected a finer
#' level of detail than either Level 1 or Level 2, Level 2 is produced first.  
#' 
#' ### Reclass table into classes using case_when and dplyr. 

#+ Do Reclass
# Adding the Level 2 Classes. 
reclassedTime1 <- addLevel2(time1)
reclassedTime2 <- addLevel2(time2)

#' #### Level 1 LULC Conversions:
#+ Adding the Level one classes.
# Add level one classes. 
reclassedTime1 <- addLevel1(reclassedTime1)
reclassedTime2 <- addLevel1(reclassedTime2)


# Process data into form that can be used for irr
# to do this, need to prepare a list of data frames with values for each class
# this is then fed to the irr functions. 

classes <- colnames(reclassedTime1)[16:38]

# Matrices for time 1
cross_tables1 <- rep(list(NA),length(classes))
names(cross_tables1) <- classes

for (m in 1:length(cross_tables1)) {
	cross_tables1[[m]] <- select(reclassedTime1, "USER_ID", "PLOT_ID", classes[m]) %>%
		spread(., "USER_ID", classes[m]) %>%
		.[,-1] %>%
		na.omit(.) %>% 
		as.matrix(.)
}


# Matrices for time 2
cross_tables2 <- rep(list(NA),length(classes))
names(cross_tables2) <- classes

for (m in 1:length(cross_tables2)) {
	cross_tables2[[m]] <- select(reclassedTime2, "USER_ID", "PLOT_ID", classes[m]) %>%
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
#'  nominal multivariate observations. Educational and Psychological Measurement, 61, 277-289. 

#+ Iota, a multivariate metric of agreement
crossval_iota1q <- iota(cross_tables1[1:19], scaledata = "q")
crossval_iota1n <- iota(cross_tables1[20:23], scaledata = "n")
crossval_iota1q
crossval_iota1n

crossval_iota2q <- iota(cross_tables2[1:19], scaledata = "q")
crossval_iota2n <- iota(cross_tables2[20:23], scaledata = "n")
crossval_iota2q
crossval_iota2n

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

#+ Per-class agreement

# Intraclass Correlation Coefficient Time 1
cross_icc1 <- list()
for (m in 1:(length(cross_tables1)-4)) {
	cross_icc1[[m]] <- icc(cross_tables1[[m]], model = "oneway", type = "agreement")
}
names(cross_icc1) <- classes[1:19]

# make a "table" from data values in list
icc_values1 <- data_frame(length(classes)-4, 5)
for (m in 1:length(cross_icc1)) {
	icc_values1[m,1] <- names(cross_icc1[m])
	icc_values1[m,2] <- round(cross_icc1[[m]]$value, 4)
	icc_values1[m,3] <- round(cross_icc1[[m]]$lbound, 4)
	icc_values1[m,4] <- round(cross_icc1[[m]]$ubound, 4)
	icc_values1[m,5] <- round(cross_icc1[[m]]$p.value, 4)
}

colnames(icc_values1) <- c("Class", "ICC", "Lower", "Upper", "Pvalue")

# Make a nice table for time 1 values. 
kable(icc_values1)

#' Now that we have the dominant landscape element classes, we can check for
#'  agreement between the interpreters on that, in this case just using simple
#'  percentages.

#+ TopClassAgreement1
# Get raw percentage agreement on dominant class
round(sum(cross_tables1$Primary[,1] == cross_tables1$Primary[,2]) 
			/ nrow(cross_tables1$Primary) * 100, 2)

# Get raw percentage agreement on secondary class. 
round(sum(cross_tables1$Secondary[,1] == cross_tables1$Secondary[,2]) 
			/ nrow(cross_tables1$Secondary) * 100, 2)

# Get raw percentage agreement on level 2 class
round(sum(cross_tables1$LEVEL2[,1] == cross_tables1$LEVEL2[,2]) 
			/ nrow(cross_tables1$LEVEL2) * 100, 2)

# Get raw percentage agreement on level 1 class. 
round(sum(cross_tables1$LEVEL1[,1] == cross_tables1$LEVEL1[,2]) 
			/ nrow(cross_tables1$LEVEL1) * 100, 2)

# Intraclass Correlation Coefficient Time 2
cross_icc2 <- list()
for (m in 1:(length(cross_tables2)-4)) {
	cross_icc2[[m]] <- icc(cross_tables2[[m]], model = "oneway", type = "agreement")
}
names(cross_icc2) <- classes[1:19]

# make a "table" from data values in list
icc_values2 <- data_frame(length(classes)-4, 5)
for (m in 1:length(cross_icc2)) {
	icc_values2[m,1] <- names(cross_icc2[m])
	icc_values2[m,2] <- round(cross_icc2[[m]]$value, 4)
	icc_values2[m,3] <- round(cross_icc2[[m]]$lbound, 4)
	icc_values2[m,4] <- round(cross_icc2[[m]]$ubound, 4)
	icc_values2[m,5] <- round(cross_icc2[[m]]$p.value, 4)
}

colnames(icc_values2) <- c("Class", "ICC", "Lower", "Upper", "Pvalue")

# Make a nice table for time 2 values. 
kable(icc_values2)

#' Now that we have the dominant landscape element classes, we can check for
#'  agreement between the interpreters on that, in this case just using simple
#'  percentages.

#+ TopClassAgreement2
# Get raw percentage agreement on dominant class
round(sum(cross_tables2$Primary[,1] == cross_tables2$Primary[,2]) 
			/ nrow(cross_tables2$Primary) * 100, 2)

# Get raw percentage agreement on secondary class. 
round(sum(cross_tables2$Secondary[,1] == cross_tables2$Secondary[,2]) 
			/ nrow(cross_tables2$Secondary) * 100, 2)

# Get raw percentage agreement on level 2 class
round(sum(cross_tables2$LEVEL2[,1] == cross_tables2$LEVEL2[,2]) 
			/ nrow(cross_tables2$LEVEL2) * 100, 2)

# Get raw percentage agreement on level 1 class. 
round(sum(cross_tables2$LEVEL1[,1] == cross_tables2$LEVEL1[,2]) 
			/ nrow(cross_tables2$LEVEL1) * 100, 2)

# Agreement on change. 
 
finalTable <- metadata
finalTable$T1L1 <- reclassedTime1$LEVEL1
finalTable$T1L2 <- reclassedTime1$LEVEL2
finalTable$T2L1 <- reclassedTime2$LEVEL1
finalTable$T2L2 <- reclassedTime2$LEVEL2
finalTable$changeL1 <- finalTable$T1L1 != finalTable$T2L1
finalTable$changeL2 <- finalTable$T1L2 != finalTable$T2L2

# produce final classes
finalTable <- addFinal(finalTable)

# make a little tibble with just the final class, spread by rater
finalAgree <- select(finalTable, "USER_ID", "PLOT_ID", "refClass") %>%
	spread(., "USER_ID", "refClass") %>%
	na.omit(.)

# Get raw percentage agreement on change class
round(sum(finalAgree[,2] == finalAgree[,3]) 
			/ nrow(finalAgree) * 100, 2)

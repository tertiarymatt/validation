#' ---
#' title: "QA/QC for Validation Data"
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
#' Each worker or team should have the data they produced for the training data
#' sets imported. There should be no need to process the data exported from CEO
#' prior to working with it in this script. 

#+ Import
# add a line for each worker or team. 
worker1 <- read_csv("data/ceo-training-project-3.1-plot-data-2019-01-07.csv")
worker2 <- read_csv("data/ceo-training-project-3.2-plot-data-2019-01-07.csv")

#' ### Process data into form that can be used for irr  
#' The imported data needs to be assembled and then cleaned up to make it easier
#' to read and work with. 

#+ Process
# make into one data frame. 
crossData <- bind_rows(worker1, worker2)

# Find and extract the class names. 
names(crossData)

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

# Now that the data is cleaned up, it needs to be processed into a form that 
# can be used for by the `irr` package. To do this, need to prepare a list of 
# single class matrices with the PI values produced by each worker separated 
# into columns. 

#+ PrepforIRR

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

#+ Per-class agreement

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
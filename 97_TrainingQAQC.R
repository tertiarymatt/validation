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
library(stargazer)
source("00_functions.R")

#' ### Import Data
#+ Import
kim <- read_csv("data/ceo-training-project-kim-plot-data-2019-01-02.csv")
steve <- read_csv("data/ceo-training-project-steve-plot-data-2019-01-02.csv")

crossData <- rbind(kim, steve)

#' ### Process data into form that can be used for irr  
#' Provide overview of the process. 

#+ Process
# Find and extract the class names. 
names(crossData)
classes <- colnames(crossData[17:35]) %>% 
	str_split(., coll(":"), simplify = TRUE) %>% 
	.[,2] %>% 
	gsub(" ", "_", .)

colnames(crossData)[17:35] <- classes
names(crossData)

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

#' ### Calculate Metrics of Agreement  
#' Describe metrics used, provide citations. 

#' IRR Metrics
#' Iota is used to calculate overall agreement between two raters. 
#
#+ Iota, a multivariate metric of agreement
crossval_iota <- iota(cross_tables, scaledata = "q")
crossval_iota

#' For checking agreement of individual classes, we can use several approaches
#' The Intraclass correlcation coefficient and mean bivariate Pearson's are two.

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


#Mean Bivariate Pearson's
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


bind_cols(icc_values, cor_values[,2:3])

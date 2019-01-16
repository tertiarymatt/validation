#' ---
#' title: "Functions"
#' author: "MS Patterson, tertiarymatt@gmail.com"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' output: github_document
#' ---
#'

#' ### Required packages
#+ Packages
library(tidyverse)

#' ### Functions related to sample size determination
#' The following sample size calculation functions are derived from the work of
#' _Foody, G. M. (2009). Sample size determination for image classification
#' accuracy assessment and comparison. International Journal of Remote Sensing,
#' 30(20), 5273-5291._ https://doi.org/10.1080/01431160903130937 
#'
#' Each of the three sample size caclulation functions relates to a particular
#' approach. `genSample1()` is an implementation of the typical sample size
#' calcuation, using only the target accuracy (p0), the half-width of the
#' Confidence Interval (h), and the tolerance for Type I error (alpha).
#'
#' `genSample2()` is used when it is desired to be able to reliably test for a
#' result being a certain distance from the target accuracy. It requires the
#' minimum detectable difference (min_diff), and also that tolerance for Type II
#' errors be specified (beta).
#'
#' `genSample3()` is used when a confidence interval is of more interest than
#' testing against a target accuracy. See eq. 22-25. This function requires the
#' specification of the target or expected accuracy (p0), alpha and beta, and
#' the difference in accuracy that can be detected with a power of 1 - beta (d).
#'
#' The function `contCorrect()` performs a continuity correction on sample size
#' estimates. A continuity correction may be necessary when using equations that
#' assume a continious distribution (samples are discrete), which results in a 
#' slight underestimate of the necessary sample size. It is most appropriate to 
#' apply to the estimate of sample size produced by `genSample2()`.
#' 
#' The function `powerEst()` can be used to estimate the power of a sample size,
#' given the minimum difference to be detected (min_diff), the expected accuracy (p0), 
#' and alpha.

#+ SampleSizeCode
genSample1 <- function(p0 = 0.85, h = 0.01, alpha = 0.05){
  # convert the input probalities into z scores
  z_alpha <- qnorm((1 - alpha/2))
  # calculate n_prime sample size
  n <- z_alpha^2 * (p0 * (1 - p0)) / (h^2)
  
  return(round(n))
}

genSample2 <- function(p0 = 0.85, min_diff = 0.01, alpha = 0.05, beta = 0.2){
  # convert the input probalities into z scores
  z_alpha <- qnorm((1 - alpha))
  z_beta <-  qnorm((1 - beta))
  
  # calculate the actual n prime estimate 
  p1 <- p0 - min_diff
  noom <- (z_alpha * sqrt(p0 * (1 - p0))) + (z_beta * sqrt(p1 * (1 - p1)))
  n <- (noom / min_diff)^2
  
  return(round(n))
}


genSample3 <-  function(p0 = 0.85, d = 0.1, alpha = 0.05, beta = 0.2){
  z_alpha <- qnorm((1 - alpha/2))
  z_beta <-  qnorm((1 - beta))
  
  n <- (2 * p0 * (1 - p0) * (z_alpha + z_beta)^2) / d^2
  
  return(round(n))
}

contCorrect <- function(n_prime, min_diff = 0.01, p0 = 0.85){
  n <- (n_prime / 4) * (1 + sqrt(1 + 2 / (n_prime * min_diff)))^2
  
  return(round(n))
}

powerEst <- function(n, min_diff, p0, alpha=0.05){
  p1 <- p0 - min_diff
  z_alpha <- qnorm((1 - alpha) + 0.025)
  noom <- sqrt(n) * min_diff - (1 / (2 * sqrt(n))) - 
          z_alpha * sqrt(p0 * min_diff)
  denoom <- sqrt(p1 * (1 - p1))
  pow <- pnorm(noom/denoom)
  return(pow)
}

 

#' ### Optimizing sample split
#' This section relies on work in _Wagner J.E. and S.V. Stehman. 2015,
#' Optimizing sample size allocation to strata for estimating area and map
#' accuracy. Remote Sens. Environ. 168:126-133_.  
#'
#' #### Error matrix checker  
#' 
#' This function checks that the area-proportion error matrix is properly 
#' formed, and accounts for 100% of the area.  

#+ ErrorMatrixChecker
checkErrorMatrix <- function(errorMatrix){
  checkRows <- sum(rowSums(errorMatrix))
  checkCols <- sum(colSums(errorMatrix))
  total <- checkRows + checkCols
  message <- ifelse(total == 2, c("Error matrix appears correctly formed."),
                    c("Errormatrix is not correctly formed."))
  return(message)
}


#' ####Two Category Optimization of Sample Distribution
#' 
#' This function uses Wagner & Stehman's approach to optimize sample 
#' distribution for two strata. The optimization is intended to minimize 
#' standard errors while maintaining accuracy of area estimation.  
#' 
#' The function requires two inputs:  
#' **errorMatrix**: An area-based error matrix for a two class map (2x2).  
#' **nTotal**: The total sample pool to work with.

#+ OptimizeSplit
optimizeSplit <-  function(errorMatrix, nTotal, classes){
  # Check that the matrix proportions work out. 
  checkErrorMatrix(errorMatrix)
  
  # Calculate individual components used for finding proportions. 
  v11 <- (errorMatrix[1,1] * (sum(errorMatrix[1,]) - errorMatrix[1,1]) / 
            sum(errorMatrix[1,])^2)
  
  v21 <- ((errorMatrix[1,1] * errorMatrix[2,1]) * 
            (errorMatrix[2,1]*errorMatrix[1,2]))/sum(errorMatrix[,1])^4
  v22 <- ((errorMatrix[1,1] * errorMatrix[2,1]) * 
            (errorMatrix[1,1]*errorMatrix[2,2]))/sum(errorMatrix[,1])^4
  
  v31 <- prod(errorMatrix[1,])
  v32 <- prod(errorMatrix[2,])
  
  k1 <-  sum(c(v11, v21, v31))
  k2 <- sum(c(v22, v32))
  
  minV <- ifelse(k1 == 0, k2, ifelse(k2 == 0, k1, min(k1, k2)))
  
  # Calculate the proportions for each class
  n1p <- k1/minV
  n2p <- sqrt(k2/minV)
  np <-  sum(c(n1p, n2p))
  
  # Calculate sample sizes for each class
  n1 <- round((n1p / np) * nTotal)
  n2 <- round((n2p / np) * nTotal)
  
  samples <- c(n1, n2)
  names(samples) <- classes
  
  return(samples)
}

#' ### CEO Point Table Reclassification Functions
#'
#' `addTopClasses` takes a raw point table produced by Collect Earth Online, and
#' returns that table with a Primary and Secondary class field added. The
#' Primary field is the class with the highest percentage cover, and the
#' Secondary is the class with the second highest. If the plot has been flagged,
#' these fields are marked FLAGGED.Ties return the classes in the order 
#' encountered. 

addTopClasses <- function(inTable = NULL, plotfield = 1, flagfield = 6, 
                       classfields = NULL){
  ### inTable should be a point-based classification table (output from CEO), 
  ### plotField should point to the PLOTID field, 
  ### flagfield should point to FLAGGED field,
  ### classfields should be a vector of the indices of the fields for the
  ### classes.
  
  require(tidyr)
  classes <- colnames(inTable)[classfields]
  plots <- select(inTable, plotfield, flagfield, classfields)
  
  #initialize variables to collect primary and secondary class types
  primary <- NULL
  secondary <- NULL
  
  for (i in 1:nrow(plots)) {
    
    if (plots[i,2] == "FALSE") {
      
      #produce a version of the table
      pl <- plots[i,-1]
      pl <- as_vector(pl[-1])
      n <- length(unique(pl))
      
      #Is there a tie?
      if (length(which(pl == sort(unique(pl))[n])) == 1) {
        primary[i] <- classes[which(pl == sort(unique(pl))[n])]
        
        #Does the second highest cover has a score of zero?
        secondary[i] <- ifelse(sort(unique(pl))[n - 1] == 0,
                               #if so, enter max again
                               classes[which.max(pl)], 
                               #if not enter second highest
                               classes[which(pl == sort(unique(pl))[n - 1])]) 
      } #in case of tie, add the tied classes, 
        #primary is just the first field encountered
      else {
        tie <- classes[which(pl == sort(unique(pl))[n])]
        paste("Plot", plots[i,1], "has a tie, with values", tie[1], "and", tie[2])
        primary[i] <- tie[1]
        secondary[i] <- tie[2]
      }
    }
    else {
      primary[i] <- "FLAGGED"
      secondary[i] <- "FLAGGED"
    }
  }
  
  inTable$Primary <- primary
  inTable$Secondary <- secondary
  
  return(inTable)
}

#' #### Adding LULC Classes  
#' The following functions build on the point-based data collected in CEO after,
#' it has been processed using `addTopClasses`. They convert the point-based
#' classes into land use and land cover (LULC) classes, using thresholds
#' devised in the LULC classification system.  
#' 
#' #### Level 2 Class Thresholds:  
#' (note these will be updated to Spanish class names)  
#' Primary Forest = Primary tree >= 30%   
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
#' Glacier = Snow/Ice >= 70%   

#+ Level2Classes
# Adding the Level 2 Classes. 
addLevel2 <- function(table){
	require(tidyr)
	reclassed <- table %>% 
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
					BARE_GROUND >= 70 ~ "Glacier",
				OTHER >= 50 ~ "Other",
				CLOUDS_UNINTERPRETABLE >= 50 ~ "No_Data",
				Primary == "FLAGGED" ~ "No_Data",
				TRUE ~ "Mosaic"
			)
		)
	return(reclassed)
}

#' #### Level 1 LULC Conversions:
#' Forest Lands = Primary, Secondary, Plantation, Mangrove  
#' Grasslands = Herbland, Shrubland, Paramo  
#' Croplands = Cropland  
#' Wetlands = Aritifical Water, Natural Water  
#' Settlements = Settlement, Infrastructure  
#' Other Lands = Glacier, Non-vegetated, Other, Mosaic  
#' No Data = No Data  

# Adding the Level one classes.
addLevel1 <- function(table){
	require(tidyr)
	reclassed <- table %>% 
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
				LEVEL2 == "Glacier" |
					LEVEL2 == "Non-vegetated" |
					LEVEL2 == "Other" |
					LEVEL2 == "Mosaic" ~ "Other_Lands",
				LEVEL2 == "No_Data" ~ "No_Data"
			)
		)
	return(reclassed)
}

#' #### Map class conversions  
#' The class of each sample point is collected as an intergers when the sample 
#' is generated in Google Earth Engine. These interger codes need to be 
#' converted into text (and later, a factor) for building an error matrix.
#' 
#' __Land cover class codes__  
#' AREA SIN COBERTURA VEGETAL:0  
#' ARTIFICIAL:1  
#' BOSQUE NATIVO:2  
#' CULTIVO ANUAL:3  
#' CULTIVO PERMANENTE:4  
#' CULTIVO SEMIPERMANENTE:5  
#' INFRAESTRUCTURA:6   
#' MOSAICO AGROPECUARIO:7   
#' NATURAL:8   
#' PARAMOS:9  
#' PASTIZAL:10   
#' PLANTACION FORESTAL:11  
#' VEGETACION ARBUSTIVA:12  
#' VEGETACION HERBACEA:13  
#' ZONAS POBLADAS:14  
#' GLACIAL:15
#' 
#+ Convert GEE codes in the exported table into class values. 
# Adding the Level 2 Classes.
# Note that the codes below are temporary, and for script development,
# and will be replaced when the LULC change product is produced. 
convertToClasses <- function(table){
	require(tidyr)
	reclassed <- table %>% 
		mutate(
			MapClass = case_when(
				CLASS == 0 ~ "Non-vegetated",
				CLASS == 1 ~ "Artificial_Water",
				CLASS == 2 ~ "Primary_Forest",
				CLASS == 3 ~ "Cropland",
				CLASS == 4 ~ "Cropland",
				CLASS == 5 ~ "Cropland",
				CLASS == 6 ~ "Infrastructure",
				CLASS == 7 ~ "Cropland",
				CLASS == 8 ~ "Natural_Water",
				CLASS == 9 ~ "Paramo",
				CLASS == 10 ~ "Herbland",
				CLASS == 11 ~ "Plantation_Forest",
				CLASS == 12 ~ "Shrubland",
				CLASS == 13 ~ "Herbland",
				CLASS == 14 ~ "Settlement",
				CLASS == 15 ~ "Glacier"
			)
		)
	return(reclassed)
}

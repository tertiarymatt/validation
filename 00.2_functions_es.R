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

#' ### GEE Map class conversions  
#' The class of each sample point is collected as an intergers when the sample 
#' is generated in Google Earth Engine. These interger codes need to be 
#' converted into text (and later, a factor) for building an error matrix.
#' 
#' __Land cover class codes__  
#' - AREA SIN COBERTURA VEGETAL:0  
#' - ARTIFICIAL:1  
#' - BOSQUE NATIVO:2  
#' - CULTIVO ANUAL:3  
#' - CULTIVO PERMANENTE:4  
#' - CULTIVO SEMIPERMANENTE:5  
#' - INFRAESTRUCTURA:6   
#' - MOSAICO AGROPECUARIO:7   
#' - NATURAL:8   
#' - PARAMOS:9  
#' - PASTIZAL:10   
#' - PLANTACION FORESTAL:11  
#' - VEGETACION ARBUSTIVA:12  
#' - VEGETACION HERBACEA:13  
#' - ZONAS POBLADAS:14  
#' - GLACIAL:15
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
				PL_CLASS == 0 ~ "Area_Sin_Cobertura_Vegetal",
				PL_CLASS == 1 ~ "Artificial",
				PL_CLASS == 2 ~ "Bosque",
				PL_CLASS == 3 ~ "Cultivo",
				PL_CLASS == 4 ~ "Cultivo",
				PL_CLASS == 5 ~ "Cultivo",
				PL_CLASS == 6 ~ "Infraestructura",
				PL_CLASS == 7 ~ "Cultivo",
				PL_CLASS == 8 ~ "Natural",
				PL_CLASS == 9 ~ "Paramos",
				PL_CLASS == 10 ~ "Vegetacion_Herbacea",
				PL_CLASS == 11 ~ "Plantacion_Forestal",
				PL_CLASS == 12 ~ "Vegetacion_Arbustiva",
				PL_CLASS == 13 ~ "Vegetacion_Herbacea",
				PL_CLASS == 14 ~ "Area_Poblada",
				PL_CLASS == 15 ~ "Glaciar"
			)
		)
	return(reclassed)
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
#' - Primary Forest = Primary tree >= 30%   
#' - Secondary Forest = Secondary tree >= 30%    
#' - Plantation = Plantation tree >= 30%   
#' - Mangrove = Mangrove >= 30%   
#' - Grass/herbland = Herbaceous veg > 0% & Tree < 30% & Shrub < 30%   
#' - Shrubland = Shrub vegetation >= 30%, Tree < 30%   
#' - Paramo = Paramo > 0%   
#' - Cropland = Crops >= 50%    
#' - Water = Natural water >= 50% | Wetland vegetation >= 50%  
#' - Settlement = Houses & Commercial >= 30% | Urban vegetation >= 30%   
#' - Infrastructure = Roads and Lots >= 30% | Infrastructure building >= 30%   
#' - Non-vegetated = barren >= 70%   
#' - Glacier = Snow/Ice >= 70%   

#+ Level2Classes
# Adding the Level 2 Classes. 
addLevel2 <- function(table){
	require(tidyr)
	reclassed <- table %>% 
		mutate(
			LEVEL2 = case_when(
				ARBOL_PRIMARIO >= 30 ~ "Bosque",
				ARBOL_SECUNDARIO >= 30 ~ "Bosque",
				ARBOL_DE_PLANTACION >= 30 ~ "Plantacion_Forestal",
				ARBOL_DE_MANGLE >= 30 ~ "Manglar",
				VEGETACION_HERBACEA_PASTOS >= 30 ~ "Vegetacion_Herbacea",
				VEGETACION_ARBUSTIVA >= 30 ~ "Vegetacion_Arbustiva",
				VEGETACION_DE_PARAMO > 0 ~ "Paramos",
				CULTIVOS >= 50 ~ "Cultivo",
				AGUA_NATURAL + 
					VEGETACION_DE_HUMEDALES >= 50 ~  "Natural",
				AGUA_ARTIFICIAL + 
					VEGETACION_DE_HUMEDALES >= 50 ~  "Artificial",
				VEGETACION_DE_HUMEDALES >= 50 & 
					AGUA_ARTIFICIAL > 0 ~ "Artificial",
				VEGETACION_DE_HUMEDALES >= 50 ~ "Natural",
				ESTRUCTURA_DE_VIVIENDA + 
					VEGETACION_DE_ASENTAMIENTOS + 
					CARRETERAS_Y_LOTES >= 30 ~ "Area_Poblada",
				CARRETERAS_Y_LOTES >= 30 & 
					ESTRUCTURA_DE_VIVIENDA > 0 ~ "Area_Poblada",
				INFRAESTRUCTURA + 
					VEGETACION_DE_ASENTAMIENTOS + 
					CARRETERAS_Y_LOTES >= 30 ~ "Infraestructura",
				CARRETERAS_Y_LOTES >= 30 ~ "Infraestructura",
				SUELO_DESNUDO >= 70 ~ "Area_Sin_Cobertura_Vegetal",
				SUELO_DESNUDO +
					ESTRUCTURA_DE_VIVIENDA +
					VEGETACION_DE_ASENTAMIENTOS >= 30 ~ "Area_Poblada",
				SUELO_DESNUDO +
					INFRAESTRUCTURA +
					VEGETACION_DE_ASENTAMIENTOS >= 30 ~ "Infraestructura",
				SUELO_DESNUDO +
					CARRETERAS_Y_LOTES >= 30 ~ "Infraestructurae",
				NIEVE_HIELO +
					SUELO_DESNUDO >= 70 ~ "Glaciar",
				OTRO >= 50 ~ "Otro",
				NUBE_ININTELIGIBLE >= 50 ~ "Sin_Datos",
				Primary == "FLAGGED" ~ "Sin_Datos",
				TRUE ~ "Mosaico"
			)
		)
	return(reclassed)
}

#' #### Level 1 LULC Conversions:
#' - Forest Lands = Primary, Secondary, Plantation, Mangrove  
#' - Grasslands = Herbland, Shrubland, Paramo  
#' - Croplands = Cropland  
#' - Wetlands = Aritifical Water, Natural Water  
#' - Settlement = Settlement, Infrastructure  
#' - Other Lands = Glacier, Non-vegetated, Other, Mosaic  
#' - No Data = No Data  

# Adding the Level one classes.
addLevel1 <- function(table){
	require(tidyr)
	reclassed <- table %>% 
		mutate(
			LEVEL1 = case_when(
				LEVEL2 == "Bosque" |
					LEVEL2 == "Plantacion_Forestal" |
					LEVEL2 == "Manglar" ~ "Bosque",
				LEVEL2 == "Vegetacion_Herbacea" | 
					LEVEL2 == "Vegetacion_Arbustiva" | 
					LEVEL2 == "Paramos" ~ "Vegetacion_Arbustiva_Y_Herbacea",
				LEVEL2 == "Cultivo" ~ "Tierra_Agropecuaria",
				LEVEL2 == "Natural" |
					LEVEL2 == "Artificial" ~ "Cuerpo_de_Agua",
				LEVEL2 == "Area_Poblada" |
					LEVEL2 == "Infraestructura" ~ "Zona_Antropica",
				LEVEL2 == "Glaciar" |
					LEVEL2 == "Area_Sin_Cobertura_Vegetal" |
					LEVEL2 == "Otro" |
					LEVEL2 == "Mosaico" ~ "Otras_Tierras",
				LEVEL2 == "Sin_Datos" ~ "Sin_Datos"
			)
		)
	return(reclassed)
}

#' #### Final class production  
#' The final land cover class is produced from both time steps. If change has
#' taken place, then a change class is assigned. If change has not occurred,
#' then the stable class is retained. The final classes and their numerical
#' codes are listed below (subject to update and change).
#' 
#' __Final LULC classes and codes__  
#' 
#' _Stable Level 2 Classes_  
#' - Bosque Primario -> Bosque Primario = 0  
#' - Bosque Secundario -> Bosque Secundario = 1  
#' - Plantacion Forestal -> Plantacion Forestal = 2  
#' - Manglar -> Manglar = 3  
#' - Vegetacion Arbustiva -> Vegetacion Arbustiva = 4  
#' - Paramo -> Paramo = 5  
#' - Vegetacion herbacea -> Vegetacion herbacea = 6  
#' - Cultivo -> Cultivo = 7   
#' - Cuerpo de Agua Natural -> Cuerpo de Agua Natural = 8   
#' - Cuerpo de Agua Artificial -> Cuerpo de Agua Artificial = 9  
#' - Area Poblada -> Area Poblada = 10   
#' - Infraestructura -> Infraestructura = 11  
#' - Area sin Cobertura Vegetal -> Area sin Cobertura Vegetal = 12  
#' - Glaciar-> Glaciar = 13  
#' 
#' _Stable Level 1 Classes (internal change)_  
#' - BosquePastizal -> Bosque (GG) = 15  
#' - Zona  Antropica -> Zona  Antropica (SS) = 16  
#' - Cuerpo de Agua -> Cuerpo de Agua (WW) = 17  
#' - Otros Tierras -> Otros Tierras (OO) = 18  
#' 
#' _Change Classes_  
#' - Bosque -> Tierra Agropecuaria (FC) = 19  
#' - Bosque -> Vegetacion arbustiva y Herbacea (FG) = 20  
#' - Bosque -> Zona  Antropica (FS) = 21  
#' - Bosque -> Cuerpo de Agua (FW) = 22   
#' - Tierra Agropecuaria -> Vegetacion arbustiva y Herbacea (CG) = 23   
#' - Tierra Agropecuaria -> Bosque (CF) = 24   
#' - Tierra Agropecuaria -> Zona  Antropica (CS) = 25  
#' - Vegetacion arbustiva y Herbacea -> Tierra Agropecuaria (GC) = 26  
#' - Vegetacion arbustiva y Herbacea -> Tierras Forestales (GF) = 27  
#' - Vegetacion arbustiva y Herbacea -> Zona  Antropica (GS) = 28  
#' - Cuerpo de Agua -> Tierra Agropecuaria (WC) = 29  
#' - Cuerpo de Agua -> Zona  Antropica (WS) = 30  
#' - Otros Tierras -> Zona  Antropica (OS) = 31  
#' - Todos los Otros Cambios (Catchall) = 32  

# Note that the code below is temporary, and for script development,
# and will be adjusted when the LULC change product is produced. 

addFinal <- function(table){
	require(tidyr)
	reclassed <- table %>% 
		mutate(
			refClass = case_when(
				T1L2 == T2L2 ~ T2L2,
				T1L1 == T2L1 ~ T2L1,
				T1L1 == "Bosque" & 
					T2L1 == "Tierra_Agropecuaria" ~ "FC",
				T1L1 == "Bosque" & 
					T2L1 == "Vegetacion_Arbustiva_Y_Herbacea" ~ "FG",
				T1L1 == "Bosque" & 
					T2L1 == "Zona_Antropica" ~ "FS",
				T1L1 == "Bosque" & 
					T2L1 == "Cuerpo_de_Agua" ~ "FW",
				T1L1 == "Tierra_Agropecuaria" & 
					T2L1 == "Bosque" ~ "CF",
				T1L1 == "Tierra_Agropecuaria" & 
					T2L1 == "Vegetacion_Arbustiva_Y_Herbacea" ~ "CG",
				T1L1 == "Tierra_Agropecuaria" & 
					T2L1 == "Zona_Antropica" ~ "CS",
				T1L1 == "Vegetacion_Arbustiva_Y_Herbacea" & 
					T2L1 == "Tierra_Agropecuaria" ~ "GC",
				T1L1 == "Vegetacion_Arbustiva_Y_Herbacea" & 
					T2L1 == "Bosque" ~ "GF",
				T1L1 == "Vegetacion_Arbustiva_Y_Herbacea" & 
					T2L1 == "Zona_Antropica" ~ "GS",
				T1L1 == "Cuerpo_de_Agua" & 
					T2L1 == "Tierra_Agropecuaria" ~ "WC",
				T1L1 == "Cuerpo_de_Agua" & 
					T2L1 == "Zona_Antropica" ~ "WS",
				T1L1 == "Otras_Tierras" & 
					T2L1 == "Zona_Antropica" ~ "OS",
				TRUE ~ "Catchall"
			)
		)
	return(reclassed)
}
Functions
================
MS Patterson, <tertiarymatt@gmail.com>
December 13, 2018

Required packages

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 2.2.1     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.5
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

### This section is for functions related to sample size determination

Text goes here describing each function.

``` r
genSample1 <- function(p0 = 0.85, h = 0.01, alpha = 0.95){
  # convert the input probalities into z scores
  z_alpha <- qnorm(alpha + 0.025)
  # calculate n_prime sample size
  n_prime <- z_alpha^2 * (p0 * (1 - p0)) / (h^2)
  
  return(round(n_prime))
}

genSample2 <- function(p0 = 0.85, min_diff = 0.01, alpha = 0.95, beta = 0.95){
  # convert the input probalities into z scores
  z_alpha <- qnorm(alpha + 0.025)
  z_beta <-  qnorm(beta + 0.025)
  
  # calculate the actual n prime estimate 
  p1 <- p0 - min_diff
  noom <- (z_alpha * sqrt(p0 * (1 - p0))) + (z_beta * sqrt(p1 * (1 - p1)))
  n_prime <- (noom / min_diff)^2
  
  return(round(n_prime))
}

contCorrect <- function(n_prime, min_diff = 0.01, p0 = 0.85){
  n <- (n_prime / 4) * (1 + sqrt(1 + 2 / (n_prime * min_diff)))^2
  
  return(round(n))
}

powerEst <- function(n, min_diff, p0, alpha=0.95){
  p1 <- p0 - min_diff
  z_alpha <- qnorm(alpha + 0.025)
  noom <- sqrt(n) * min_diff - (1 / (2 * sqrt(n))) - 
          z_alpha * sqrt(p0 * min_diff)
  denoom <- sqrt(p1 * (1 - p1))
  pow <- pnorm(noom/denoom)
  return(pow)
}
```

### Optimizing sample split

This section relies on work in Wagner J.E. and S.V. Stehman. 2015, Optimizing sample size allocation to strate for estimating area and map accuracy. Remote Sens. Environ. 168:126-133.

#### Error matrix checker

This function checks that the area-proportion error matrix is properly formed.

``` r
checkErrorMatrix <- function(errorMatrix){
  checkRows <- sum(rowSums(errorMatrix))
  checkCols <- sum(colSums(errorMatrix))
  total <- checkRows + checkCols
  message <- ifelse(total == 2, c("Error matrix appears correctly formed."),
                    c("Errormatrix is not correctly formed."))
  return(message)
}
```

#### Two Category Optimization of Sample Distribution

This function uses Wagner & Stehman's approach to optimize sample distribution for two strata, to minimize SE and maintain accuracy of error estimation.

**errorMatrix**: An area-based error matrix for a two class map (2x2).
**nTotal**: The total sample pool to work with.

``` r
optimizeSplit <-  function(errorMatrix, nTotal){
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
  
  return(cat("Class 01 should get", n1, "samples.", 
             "\nClass 02 should get", n2, "samples."))
}
```

### Functions for fuzzy accuracy assessment

#### Make a fuzzy population error matrix

``` r
fuzzyTable <- function(ref = NULL, map = NULL, classes = NULL){
  ### Input two fuzzy class tables, composed of nrows = sample size, ncols =
  ### number of classes, along with the class names as a character vector.
  ### Tables should be of identical size, with identical ordering of sample
  ### locations and classes. Fuzzy error matrix is calculated pixelwise for the
  ### two tables, and output as a labeled matrix.
  
  #establish table
  fTable <- matrix(nrow=length(classes)+1, ncol=length(classes)+1, 0)
  
  for(i in 1:nrow(cT)){
    r <- rT[i,]
    c <- cT[i,]
    
    #Build a single pixel table
    fPixel <- matrix(nrow=length(c)+1, ncol=length(r)+1)
    fPixel[length(classes)+1,] <- c(r, 0)
    fPixel[,length(classes)+1] <- c(c, sum(c))
    
    for(m in 1:length(classes)){
      for(n in 1:length(classes)){
        fPixel[m,n] <- min(r[n], c[m])
      }
    }
    
    rownames(fPixel) <- c(classes, "Grade")
    colnames(fPixel)<- c(classes, "Grade")
    fPixel
    
    #add tables
    fTable <- fTable + fPixel
  }
  return(fTable)
}
```

#### CEO Point Table Reclassification

Condense a soft classication table into two columns, primary and secondary, add to table

``` r
topClasses <- function(inTable = NULL, plotfield = 1, flagfield = 6, classfields = NULL){
  ### inTable should be a soft classification table (output from CEO), plotField
  ### should point to the PLOTID field, flagfield should point to FLAGGED field,
  ### classfields should be a vector of the indices of the fields for the
  ### classes.
  
  require(tidyr)
  classes <- colnames(inTable)[classfields]
  plots <- select(inTable, plotfield, flagfield, classfields)
  
  #initialize variables to collect primary and secondary class types
  primary <- NULL
  secondary <- NULL
  
  for(i in 1:nrow(plots)){
    
    if(plots[i,2] == FALSE){
      
      #produce a verson of the table
      pl <- plots[i,-1]
      pl <- as_vector(pl[-1])
      n <- length(unique(pl))
      
      if(length(which(pl == sort(unique(pl))[n])) == 1){ # Is there a tie?
        primary[i] <- classes[which(pl == sort(unique(pl))[n])]
        secondary[i] <- ifelse(sort(unique(pl))[n-1] == 0, # Does the second highest cover has a score of zero?
                               classes[which.max(pl)], # if so, enter max again
                               classes[which(pl == sort(unique(pl))[n-1])]) #if not enter second highest
      } else { #in case of tie, add the tied classes, primary is just the first field encountered
        tie <- classes[which(pl == sort(unique(pl))[n])]
        paste("Plot", plots[i,1], "has a tie, with values", tie[1], "and", tie[2])
        primary[i] <- tie[1]
        secondary[i] <- tie[2]
      }
    }
    else{
      primary[i] <- "FLAGGED"
      secondary[i] <- "FLAGGED"
    }
  }
  
  inTable$Primary <- primary
  inTable$Secondary <- secondary
  
  return(inTable)
}
```

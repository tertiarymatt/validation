#### Script Information ----
#### Scratch file for developing functions and scripts
#### Author: MS Patterson
#### email: tertiarymatt@gmail.com
#### last updated: 12/12/18

#### Required packages ----

#### Developing functions for optimizing sample size ----

# need an area-weighted error matrix
error <- matrix(nrow = 2, ncol = 2, 
                c(0.05, 0.0125, 
                  0.0125, 0.925))

nTotal <- 7200

checkErrorMatrix <- function(errorMatrix = NULL){
  checkRows <- sum(rowSums(errorMatrix))
  checkCols <- sum(colSums(errorMatrix))
  total <- checkRows + checkCols
  message <- ifelse(total == 2, c("Error matrix appears correctly formed."),
         c("Errormatrix is not correctly formed."))
  return(message)
}

optimizeSplit <-  function(errorMatrix){
  checkErrorMatrix(errorMatrix)

  v11 <- (error[1,1] * (sum(error[1,]) - error[1,1])/sum(error[1,])^2)
  
  v21 <- ((error[1,1] * error[2,1]) * (error[2,1]*error[1,2]))/sum(error[,1])^4
  v22 <- ((error[1,1] * error[2,1]) * (error[1,1]*error[2,2]))/sum(error[,1])^4
  
  v31 <- error[1,1] * error[1,2]
  v32 <- error[2,1] * error[2,2]
  
  k1 <-  sum(c(v11, v21, v31))
  k2 <- sum(c(v22, v32))
  
  minV <- ifelse(k1 == 0, k2, ifelse(k2 == 0, k1, min(k1, k2)))
  
  n1p <- k1/minV
  n2p <- sqrt(k2/minV)
  np <-  sum(c(n1p, n2p))
  
  n1 <- round((n1p / np) * nTotal)
  n2 <- round((n2p / np) * nTotal)
}
source("getmonitor.R")
source("complete.R")

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  vecReturn <- vector(mode="numeric")
  idxReturn <- 1
  dataComplete <- complete(directory)
  for (i in 1:nrow(dataComplete)) {
    if (dataComplete$nobs[i] > threshold) {
      dataMonitor <- getmonitor(dataComplete$id[i], directory)
      vecReturn[idxReturn] <- cor(dataMonitor$sulfate, dataMonitor$nitrate, use = "complete.obs")
      idxReturn <- idxReturn + 1
    } 
  }
  vecReturn
}
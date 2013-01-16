complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  
  dataIdx <- 1
  nobs <- vector()
  # Opening each file in target directory
  for (i in id) {
    idx <- as.numeric(i)
    idx <- sprintf("%03d", idx)
    monData <- read.csv(paste0(directory,"/",idx,".csv"), header=TRUE)
    nobs[dataIdx] <- sum(complete.cases(monData))
    dataIdx <- dataIdx + 1
  }
  
  retData <- data.frame(id,nobs)
  retData
}
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- table(data$State)
  if (missing(state) || is.na(states[state])) {
    stop("invalid state")
  }
  outcomes <- c(11, 17, 23)
  names(outcomes) <- c("heart attack", "heart failure", "pneumonia")
  if (missing(outcome) || is.na(outcomes[outcome])) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  dataSplit <- split(data, data$State)
  dataState <- dataSplit[[state]]
  
  # Removing NA values
  rate <- suppressWarnings(as.numeric(dataState[,outcomes[outcome]]))
  dataState <- subset(dataState, !is.na(rate))
  rate <- rate[!is.na(rate)]
  
  # Calulating the ranking
  ranking <- order(rate,            ## outcome
                   dataState[,2])      ## name
  
  # Returning the selected data
  if (num == "best") {
    return(dataState[ranking,2][1])
  }
  if (num == "worst") {
    return(dataState[ranking,2][length(ranking)])
  }
  if (as.numeric(num) < 1 || as.numeric(num) > length(ranking)) {
    return(NA)
  } else {
    return(dataState[ranking,2][as.numeric(num)])
  }
  
  
  
}
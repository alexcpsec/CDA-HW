rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  outcomes <- c(11, 17, 23)
  names(outcomes) <- c("heart attack", "heart failure", "pneumonia")
  if (missing(outcome) || is.na(outcomes[outcome])) {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  # Return VAlue
  hospital <- vector()
  state <- names(table(data$State))
  dataSplit <- split(data, data$State)
  
  for (i in 1:length(state)) {
    dataState <- dataSplit[[state[i]]]
    
    # Removing NA values
    rate <- suppressWarnings(as.numeric(dataState[,outcomes[outcome]]))
    dataState <- subset(dataState, !is.na(rate))
    rate <- rate[!is.na(rate)]
    
    # Calulating the ranking
    ranking <- order(rate,            ## outcome
                     dataState[,2])      ## name
    
    # Returning the selected data
    if (num == "best") {
      hospital[i] = dataState[ranking,2][1]
      next
    }
    if (num == "worst") {
      hospital[i] = dataState[ranking,2][length(ranking)]
      next
    }
    if (as.numeric(num) < 1 || as.numeric(num) > length(ranking)) {
      hospital[i] = NA
    } else {
      hospital[i] = dataState[ranking,2][as.numeric(num)]
    }
  }

  retFrame <- data.frame(hospital,state)
  rownames(retFrame) <- state
  retFrame
  
}
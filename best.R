best <- function(state, outcome) {
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
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  dataSplit <- split(data, data$State)
  dataState <- dataSplit[[state]]
  orderState <- order(suppressWarnings(as.numeric(dataState[,outcomes[outcome]])), ## outcome
                      dataState[,2])      ## name
  dataState[orderState,2][1]
}
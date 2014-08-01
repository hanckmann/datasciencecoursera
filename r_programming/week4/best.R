best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  statedata <- data[data$State == state, ]
  if(nrow(statedata) <= 0) {
    stop('invalid state')
  }
  if(outcome == 'heart attack') {
    outcome <- c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack')
  } 
  else if(outcome == 'heart failure') {
    outcome <- c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure')
  } 
  else if(outcome == 'pneumonia') {
    outcome <- c('Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')
  } else {
    stop('invalid outcome')
  }
  
  # Select usefull columns (outcome is now always column 3)
  tokeep <- c('Hospital.Name', 'State', outcome)
  statedata <- statedata[,tokeep]
  
  # Order the data on hospital-name
  statedata <- statedata[order(statedata$Hospital.Name),]
  
  # Remove NaN values
  statedata[, outcome] <- as.numeric(statedata[, outcome])
  statedata <- statedata[complete.cases(statedata), ]
  
  # Find the min hospital
  oc <- statedata[,3]
  inds = which(oc == min(oc), arr.ind=TRUE)
  
  name <- statedata[inds[1],1]
  return(name)
}
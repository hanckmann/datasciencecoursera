rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that num, state, and outcome are valid
  if((is.numeric(num)) && (num >= 1)) {
    num <- round(num)
  }
  else if(is.character(num) && (num == 'best')) {
    num <- 1
  }
  else if(is.character(num) && (num == 'worst')) {
    num <- -1
  }
  else {
    return(NA)
  }
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
  
  # Order the data on rank
  statedata <- statedata[order(statedata[,3]),]
  
  # Check for 'worst'
  if(num < 1) {
    num <- nrow(statedata)
  }
  
  # Check if the rank exists
  if(nrow(statedata) < num) {
    return(NA)
  }
  
  name <- statedata[num,1]
  return(name)
}
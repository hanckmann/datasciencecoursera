rankhospital <- function(statedata, num) {
  
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

rankall <- function(outcome, num = "best") {
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
  data <- data[,tokeep]
  
  # Remove NaN values
  data[, outcome] <- as.numeric(data[, outcome])
  data <- data[complete.cases(data), ]
  
  # Order the data on hospital-name
  data <- data[order(data$Hospital.Name),]
  
  # order is now correct, we need to aggregate the results, select, and present them
  statenames <- unique(data[,c('State')])
  hospital <- rep("", length(statenames))
  for(i in 1:length(statenames)) {
    # loop for each state
    statedata <- data[data[, c('State')]==statenames[i], ]
    
    hospital[i] <- rankhospital(statedata, num)
  }
  
  # Select result columns
  names <- data.frame(hospital=hospital, state=statenames)
  names <- names[order(names[,2]),]
  
  return(names)
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}
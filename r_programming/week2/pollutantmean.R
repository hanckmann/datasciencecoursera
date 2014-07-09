pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  # Read the files related with the ids and store as frames
  monitors <- data.frame()
  for (i in 1:length(id) ) {
    # Create the filename by adding zeros (extension and path added later)
    filename <- toString(id[i])
    while (nchar(filename) < 3)
    {
      filename <- paste0('0', filename)
    }
    filepath <- file.path(getwd(), directory, paste0(filename, '.csv'))
    # Check if the file exists
    if (!file.exists(filepath)) {
      print(paste('Error: file note found:', filepath))
    } else {
      this_monitor <- read.csv(filepath)
      monitors <- rbind(monitors, this_monitor)
    }
  }
  
  # Check if the pollutant is valid
  pollutant_mean = NA
  if(pollutant != 'sulfate' && pollutant != 'nitrate') {
    print(paste('Error: pollutant is not found (valid are sulfate and nitrate):', pollutant))
  } else {
    # Extract the values and put into a vector
    # Remove NA values
    # Calc the mean
    pollutant_values <- monitors[,pollutant]
    pollutant_values <- pollutant_values[!is.na(pollutant_values)]
    pollutant_mean <- mean(pollutant_values)
  }
  
  # Return the mean
  return(pollutant_mean)
}

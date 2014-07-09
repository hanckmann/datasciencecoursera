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
  
  
  # Read the files related with the ids and store as frames
  overview <- matrix(data=NA, nrow=length(id), ncol=2, byrow=TRUE)
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
      
      # Find the number of Completely Observed Cases
      this_monitor <- this_monitor[complete.cases(this_monitor),]
      
      overview[i,] <- c(id[i], nrow(this_monitor))
    }
  }
  
  monitors <- as.data.frame(overview)
  colnames(monitors) <- c('id', 'nobs')
  return(monitors)
}
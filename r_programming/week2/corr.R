corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  # Get the completely observed observations count and filter based on the threshold
  coo_count <- complete(directory, 1:332)
  coo_count <- coo_count[coo_count$nobs > threshold,]
  
  # Get the correlations per id/file
  correlations <- vector(length=nrow(coo_count))
  for (i in 1:nrow(coo_count) ) {
    # Create the filename by adding zeros (extension and path added later)
    filename <- toString(coo_count[i,1])
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
      
      correlations[i] <- cor(this_monitor[,c('sulfate', 'nitrate')], use="complete.obs")[1,2]
    }
  }
  
  return(correlations)
}
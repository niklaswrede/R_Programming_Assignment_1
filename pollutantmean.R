pollutantmean <- function(directory, pollutant, id = 1 : 332) {
  fileList <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  values <- numeric()
  
  for (i in id) {
    data <- read.csv(fileList[i])
    values <- c(values, data[[pollutant]])
  }
  mean(values, na.rm = TRUE)
}

complete <- function(directory, id = 1:332) {
  filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  nobs <- numeric()
  
  for (i in id) {
    data <- read.csv(filelist[i])
    nobs <- c(nobs, sum(complete.cases(data)))
  }
data.frame(id, nobs)
  
}
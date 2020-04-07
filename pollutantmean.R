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


corr <- function(directory, threshold = 0) {
  filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  correlations <- c()
  
  for (i in 1:length(filelist)) {
    data <- read.csv(filelist[i])
    no_na <- na.exclude(data)
    if(nrow(no_na) >= threshold) {
      correlations <- c(correlations, cor(no_na$nitrate, no_na$sulfate))
    }
  }
  correlations
  
}
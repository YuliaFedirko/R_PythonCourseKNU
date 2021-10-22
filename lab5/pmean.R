pmean <- function(directory, pollutant, id = 1:332){
  
  filelist <- list.files(directory, pattern=".csv", full.names=TRUE)
  values <- numeric()
  for(monitor in id){
    data <- read.csv(filelist[monitor])
    values <- c(values,data[[pollutant]])
  }
  
  mean(values,na.rm=TRUE) 
}

complete <- function(directory, id = 1:332){
  filelist <- list.files(directory, pattern=".csv", full.names=TRUE)
  df <- data.frame()
  for(monitor in id){
    data <- read.csv(filelist[monitor])
    actual_data <- data %>% select(2, 3)
    actual_data <- data[complete.cases(actual_data),]
    nobs <- nrow(actual_data)
    df <- rbind(df, data.frame(id=monitor, nobs=nobs))
  }
  
  return(df)
}

corr <- function(directory, threshold = 0){
  
  filelist <- list.files(directory,full.names = TRUE)
  result <- c()
  for(monitor in 1:332){
    data <- (read.csv(filelist[monitor]))
    actual_data <- data[complete.cases(data),]
    if(nrow(actual_data) > threshold){
      result <- c(result, cor(actual_data$sulfate,actual_data$nitrate))
    }
    else 0
    
  } 
  return(result)
  
}
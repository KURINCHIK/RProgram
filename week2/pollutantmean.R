## By Kurinchi Kumaran
library(stringr)
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating the 
  ## location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating the
  ## name of the pollutant for which we will calculate the mean; 
  ## either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vecto'r (ignoring NA values)
  
  nMean <- NA
  
  ## check 'directory'
  if (directory == "" || !file.exists(directory)) {
    cat("Please specify valid 'directory'.\n")
    return(nMean)
  }
  
  ## check values for 'pollutant'
  sPollutant <- tolower(pollutant)
  if (sPollutant != "sulfate" && sPollutant != "nitrate") {
      cat("Please specify pollutant value as 'sulfate' or 'nitrate'.\n")
      return(nMean)
  }
  
  ## check values for 'id'
  if (sum(id < 1 | id > 332) > 0)  {
    cat("Please specify the range of 'id' from 1:332'.\n")
    return(nMean)
  }
  
  ## Read the files
  nSumVal <- 1:length(id) # The sum of observance for each file
  nNoVal <- 1:length(id)  # The number of observance for each file
  nIndex <- 1L
  for (nSeqNo in id){
    
    ## Concatenate full path of the file
    sFileName <- sprintf("%s.csv", 
                         str_pad(nSeqNo, width=3, pad="0", side="left"))
    
    sDir <- dirname(file.path(directory, sFileName))
    sFileName <- file.path(sDir, sFileName) 
    
    ## Read the file
    data<-read.csv(sFileName)

    ## Filter condition
    L <- is.na(data[[pollutant]])
    dataFilter<-data[!L, ][[pollutant]]
    
    ## store the result
    nSumVal[nIndex] <- sum(dataFilter)
    nNoVal[nIndex] <- length(dataFilter)
    nIndex <- nIndex + 1
  }
  
  ## Return the mean
  sum(nSumVal)/sum(nNoVal)
}
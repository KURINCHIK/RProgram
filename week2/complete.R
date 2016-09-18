## By Kurinchi Kumaran
library(stringr)
complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating the 
  ## location of the CSV files

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the data frame
  
  ## check 'directory'
  if (directory == "" || !file.exists(directory)) {
    cat("Please specify valid 'directory'.\n")
    return(NULL)
  }
  
  ## check values for 'id'
  if (sum(id < 1 | id > 332) > 0)  {
    cat("Please specify the range of 'id' from 1:332'.\n")
    return(NULL)
  }
  
  ## Read the files
  nIdVal <- 1:length(id) # The ID number
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
    L <- !is.na(data$sulfate) & !is.na(data$nitrate)
    dataFilter<-data[L, ]
    
    ## store the result
    nIdVal[nIndex] <- nSeqNo
    nNoVal[nIndex] <- nrow(dataFilter)
    nIndex <- nIndex + 1
  }
 
  ##  return data frame
  sFinalData<-data.frame(stringsAsFactors = FALSE, nIdVal, nNoVal)
  names(sFinalData) <- c("id", "nobs")
  sFinalData
}
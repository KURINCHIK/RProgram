## By Kurinchi Kumaran
library(stringr)
corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating the 
  ## location of the CSV files

  ## 'threshold' is an integer vector of length indicating the 
  ## number of completed observations required to compute correlation
  ## between nitrate and sulfate 

  ## Return the numeric vector of correlation
  
  ## check 'directory'
  if (directory == "" || !file.exists(directory)) {
    cat("Please specify valid 'directory'.\n")
    return(NULL)
  }
  

  ## Get the data where nos > threshold
  dfComplete <- complete(directory, 1:332)
  fiterThreshold <- dfComplete$nobs > threshold
  dfCompleteFiltered <-dfComplete[fiterThreshold,]
  nRows <- nrow(dfCompleteFiltered)
   
  if (nRows == 0) {
    return(numeric(0))
  }
  
  ## Read the files
  ##nCorrVal <- vector("list", length = nRows) ## corrielation

  nCorrVal <- 1:nRows
  
  for (nSeqNo in 1:nRows){
    
    ## Concatenate full path of the file
    sFileName <- sprintf("%s.csv", 
                         str_pad(dfCompleteFiltered[nSeqNo,"id"], width=3, pad="0", side="left"))
    
    sDir <- dirname(file.path(directory, sFileName))
    sFileName <- file.path(sDir, sFileName) 

    ## Read the file
    data<-read.csv(sFileName)

    ## Filter condition
    L <- !is.na(data$sulfate) & !is.na(data$nitrate)
    dataFilter<-data[L, ]
    
    ## store the result
    nCorrVal[nSeqNo] <- cor(dataFilter$nitrate, dataFilter$sulfate)

  }

  ##  return vector
  nCorrVal
}
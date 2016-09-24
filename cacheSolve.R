## By Kurinchi Kumaran

makeCacheMatrix <- function(spmatrix = matrix()){
  
    ## 'spmatrix' is a square matrix (n x n matrix)
    ## Function creates a special matrix oject that
    ## can cache its inverse

    ## Inverse matrix
  
    invmatrix = NULL
  
    ## Function to set the matrix passed as an argument
    set <- function(nmatrix) {
        spmatrix<<-nmatrix
        invmatrix<<-NULL
    }
  
    ## Function to return the matrix
    get <- function() spmatrix

    ## Function to set the inverse matrix
    setInverseMatrix <- function(imatrix) invmatrix<<-imatrix
  
    ## Function to get the inverse matrix
    getInverseMatrix <- function() invmatrix
  
    list(set=set,
         get=get,
         setInverseMatrix=setInverseMatrix,
         getInverseMatrix=getInverseMatrix)
}

cacheSolve <- function(spmatrix,..) {
    ## 'spmatrix' is a square matrix (n x n matrix)
    ## Function computes the inverse of  special matrix
    
    ## check inverse matrix exists in cache and return if exists
    invmatrix <- spmatrix$getInverseMatrix()
    if (!is.null(invmatrix)) {
      return(invmatrix)
    }
  
    ## Compute inverse of the matrix  
    data<-spmatrix$get()
    invmatrix <- solve(data)

    ## Save inverse matrix to cache   
    spmatrix$setInverseMatrix(invmatrix)

    ## return the computed inverse matix
    invmatrix
}

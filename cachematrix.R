## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      ##setup variable
      matrixInv <<- NULL
      ##copy original matrix to allow for a comparison for changes in the future 
      matrixCopy <<- x;
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      ## Validate if InverseMatrix exists
      if (exists("matrixInv")){
            ## Validate if InverseMatrix is empty
            if(!is.null(matrixInv)) {
                  ## Validate if current matrix is the same as the one for 
                  ## which the inverse was calculated
                  if (identical(matrixCopy,x)){
                        ## return cached data
                        message("getting cached data")
                        return(matrixInv) 
                  }
            }
      }
      ## in the case for which either the cache was empty or matrix has changed
      ## we build a new cache.
      message("calculating Inverse")
      matrix <- makeCacheMatrix(x)
      ## we calculate the inverse
      matrixInv <<- solve(matrix,...) 
      ## we return the results
      return (matrixInv)
}
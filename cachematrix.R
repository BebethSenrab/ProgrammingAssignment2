## Coursera.org
##===============================================================================================
## Course           John Hopkins data science specialisation
## Module           R Programming
## Assignment       Programming Assignment 2
##===============================================================================================
## Author           Liz Barnes
## Last Amended     26/04/2015
##===============================================================================================
## Function Name    makeCacheMatrix
##===============================================================================================
## Description      Defines 4 functions for storing and retrieving a cached matrix object
##                  The <<- operator stores the return value in a cache
##                  get()         Returns the cached matrix object
##                  set()         Sets the cached matrix object
##                  getinv()
##                  setinv()
## 
## Arguments        x             A matrix object which is invertible
##                                
##
## Return Value     y             A list containing the following functions - get,set,getinv,setinv
##==============================================================================================================
makeCacheMatrix <- function(x = matrix()) {
  
  
  ## set the value of the matrix to null
  m <- NULL
  
  
  ## the get() function returns the matrix X stored in the main function
  get <- function() x
  
  ## the set() function changes the matrix X stored in the main function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  getinv <- function() m
  
  setinv <- function(solve) m <<- solve
  
  
  ## Store the above functions in a list - so they can be called
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}

##===============================================================================================
## Function Name    cacheSolve
##===============================================================================================
## Description      Return a matrix object that is the inverse of X
##                  Check if the inverse matrix is already cached and if so - return this
##                  If the inverse matrix is not already cached - then cache it first and then return it
## 
## Arguments        X             A matrix object which is invertible
##
## Return Value     m             The inverse of X
##===============================================================================================
## Unit test        source("cachematrix.R")
##                  m <- matrix(c(-1, -2, 1, 1), nrow=2,ncol=2)
##                  x<-makeCacheMatrix(m)
##                  inv <- cacheSolve(x)
##                  m
##                  inv
##                 
##Expected Result
##                      [,1] [,2]
##                [1,]   1    -1
##                [2,]   2    -1
##===============================================================================================

cacheSolve <- function(x, ...) {
  
  ## verify that m which was stored previously exists and is not null
  ## and if so return the cached value 
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  
  ##  otherwise  get the matrix 
  data <- x$get()
  
  ## invert the matrix
  m <- solve(data, ...)
  
  ## store the inverted matrix in the cache
  x$setinv(m)
  
  ## return the inverted matrix
  m        
}

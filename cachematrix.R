## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function
## Accepts a matrix as input parameter and stores the matrix in the Parent Environment
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inverse_matrix <- NULL
      
      setMatrix <- function(y) {
      ## Sets the input matrix in the Parent Environment   
      original_matrix <<- y
      ## Sets the inverse as null in the Parent Environemtn
      inverse_matrix <<- NULL
     }
    
    getMatrix <- function() original_matrix
    setInverse <- function(inverse) inverse_matrix <<- inverse
    getInverse <- function() inverse_matrix
  
    list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  
  ## Get "Inverse" from Parent Environment
  get_inverse <- x$getInverse()
  
  ## If "Inverse" is not null - meaning inverse was computed and stored, use the value
  if(!is.null(get_inverse)) {
    message("getting cached data")
    return(get_inverse)
  }
  
  ## If "Inverse" is null, get the original matrix and compute the inverse
  data <- x$getMatrix()
  
  ## Compute Inverse
  get_inverse <- solve(data, ...)
  
  ## Store the computed inverse in the Parent Environment
  x$setInverse(get_inverse)
  
  ## Display Inverse
  get_inverse
}
## Assignment: Caching the Inverse of a Matrix
## This file contains a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	
	inverse <- NULL 	## Initializing matrix inverse with null
  	set <- function(y) 	## Setting value of vector
 	 {
    		x <<- y
    		inverse <<- NULL
 	 }
  	get <- function() x 	## Retrieving the value of the matrix
  	setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
				## To set the value of the inverse matrix
  
  	getInverse <- function() inverse ## To retrive the value of the inverse matrix
  
  	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inverse <- x$getInverse() 	#Return the inverse matrix of x

  if(!is.null(inverse)){ 		## To check if the inverse matrix has already been computed
    message("getting cached data")
    return(inverse) 			## Returning the already computed inverse matrix
  }

 				 ## Computing the inverse of the matrix if not already computed
  mat1 <- x$get()
  inverse <- solve(mat1) 	## Using solve function in R to compute the inverse of matrix
  x$setInverse(inverse)
  inverse
}

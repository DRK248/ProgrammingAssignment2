## Deepank Korandla
## R Programming: Programming Assignment 2
## The functions defined in this script create a special matrix object that is
## able to cache its inverse and solve for the inverse if the matrix changes.


## This function creates a special matrix object that is able to store its
## inverse and the matrix used to solve for the inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the stored variables. They are only updated to have a non-null
  ## value when the inverse is solved. The cached matrix essentially tracks
  ## changes to the matrix between calls to solve for the inverse.
  cacheMatrix <- NULL
  inverse <- NULL
  
  ## The set function for the matrix updates the object while clearing its
  ## cached version and the inverse.
  set <- function(newMatrix) {
    x <<- y
    cacheMatrix <<- NULL
    inverse <<- NULL
  }
  
  get <- function(){x}

  ## Get and set functions for the stored matrix
  setCacheMatrix <- function(updatedMatrix){cacheMatrix <<- updatedMatrix}
  getCacheMatrix <- function(){cacheMatrix}

  ## Get and set functions for the stored inverse
  setInverse <- function(newInverse){inverse <<- newInverse}
  getInverse <- function(){inverse}
  
  ## Create and return a list that maintains the functions available to the
  ## special matrix objects for easier access.
  list(set = set, get = get, getCacheMatrix = getCacheMatrix,
       setInverse = setInverse, getInverse = getInverse)
}


## This function solves for the inverse of a given "special" matrix object,
## skipping computation if the inverse has already been solved and the matrix
## hasn't been updated.
cacheSolve <- function(x, ...) {
  ## First get the inverse and the matrix used to generate the inverse.
  inversedMatrix <- x$getCacheMatrix()
  currentInverse <- x$getInverse()
  
  ## If the inverse isn't null and th matrix hasn't been updated after
  ## calculating that inverse, return the inverse.
  if(!is.null(currentInverse) && identical(x, inversedMatrix)) {
    message("Getting cached data")
    return(currentInverse)
  }
  
  ## Otherwise, solve for the new inverse...
  data <- x$get()
  currentInverse <- solve(data)
  
  ## ...and update the inverse and matrix in the cache.
  x$setInverse(currentInverse)
  x$setCacheMatrix(data)
  
  ## Finally, return the new inverse.
  currentInverse
}

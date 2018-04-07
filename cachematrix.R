## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#' Create a special "matrix" object that can cache its inverse.
#' 
#' @param x A matrix
#' 
#' @return A list containing four functions to set and get the value of the
#'     matrix and to set and get the inverse of the matrix
#'     
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function


#' Return inverse of matrix x
#' 
#' This function computes the inverse of the special "matrix" returned by 
#' makeCacheMatrix above. If the inverse has already been calculated 
#' (and the matrix has not changed), then the cachesolve retrieves the 
#' inverse from the cache.
#' 
#' @param x a special matrix created with makeCacheMatrix
#' 
#' @return The inverse of the matrix x
#' 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

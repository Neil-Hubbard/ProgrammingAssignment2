## This is a pair of functions:
## makeCacheMatrix first creates a matrix and stores it to cache
## It can also store the inverse to the cached matrix, 
## but it doesn't calculate it
##
## cacheSolve picks up the cached matrix from makeCacheMatrix
## if the inverse is already stored it doesn't "solve" it again, 
## instead just returns it
## otherwise it solves and stores the inverse in the cache


## makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse.
## it also creates a list of 4 functions, set, get, setsolve and getsolve

makeCacheMatrix <- function(n = matrix()) {
  o <- NULL
  set <- function(p) {
    n <<- p
    o <<- NULL
  }
  get <- function() n
  setsolve <- function(solve) o <<- solve
  getsolve <- function() o
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}

## Example use is:
## B = makeCacheMatrix(matrix(c(2, 4, 3, 1, 5, 7, 3, 5, 4), nrow=3, ncol=3))
##
## [,1] [,2] [,3]
## [1,]    2    1    3
## [2,]    4    5    5
## [3,]    3    7    4

## ****************************************************************************

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

## I am assuming that the "matrix hasn't changed" is a given here, as 
## as deliberate intervention would need to occur to change the cached value

cacheSolve <- function(n, ...) {
  o <- n$getsolve()
  if(!is.null(o) ## & n == p
  ) {
    message("getting cached data")
    return(o)
  }
  data <- n$get()
  
  ## Return a matrix that is the inverse of 'n'
  ## Computing the inverse of a square matrix can be done with the solve
  ## function in R. For example, if n is a square invertible matrix, then
  ## solve(n) returns its inverse.
  
  o <- solve(data, ...)
  n$setsolve(o)
  o
  
}

## Example use is:
## B = makeCacheMatrix(matrix(c(2, 4, 3, 1, 5, 7, 3, 5, 4), nrow=3, ncol=3))
##  cacheSolve(B)
##
## [,1]   [,2]  [,3]
## [1,] -1.875  2.125 -1.25
## [2,] -0.125 -0.125  0.25
## [3,]  1.625 -1.375  0.75
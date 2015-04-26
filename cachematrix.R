## Creates a cacheable matrix from an R matrix

makeCacheMatrix <- function(matrix) {
  inverse <- NULL
  list(
    set = function(aMatrix) {
        matrix <<- aMatrix
        inverse <<- NULL
        }, 
    get = function() matrix,
    setinverse = function(aNumber) inverse <<- aNumber,
    getinverse = function() inverse)
}

## Calculates and caches the inverse of a cacheable matrix

cacheSolve <- function(cacheMatrix, ...) {
  inverse <- cacheMatrix$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  inverse <- solve(cacheMatrix$get(), ...)
  cacheMatrix$setinverse(inverse)
  inverse
}

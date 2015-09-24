## Calculate a matrix inversion and cahce the result
## On subsequent calls, return the cached result if the input hasn't changed

## create the functions to cache a matrix inversion.  When the inpout changes, set the cached matrix inversion to null to 
## indicate that the inversion needs to be recalculated

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvert <- function(invert) i <<- invert
  getinvert <- function() i
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}

## use the above functions to calculate a matrix inversion, using a cached copy if possible

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinvert()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinvert(i)
  i
}

## There are two function here. 
## makeCacheMatrix(x) make the matrix with cached inversion.
## cacheSolve(x) get the inversion of matrix x, while the inversion only need to be calculated after matrix changed.


## make a matrix with cached inversion  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInversion <- function(inversion) m <<- inversion
  getInversion <- function() m
  list(set = set, get = get,
       setInversion = setInversion,
       getInversion = getInversion)
}


## get the inversion of matrix x, while the inversion only need to be calculated after matrix changed.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInversion()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInversion(m)
  m
}

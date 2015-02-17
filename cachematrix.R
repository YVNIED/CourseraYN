## These functions allows to cache the inverse of a matrix, 
## so it does not need to be computed repeatedly

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { ## setting the values of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x ## getting the values of x
  setsolve <- function(solve) m <<- solve ## setting the solve of matrix
  getsolve <- function() m ## getting the solve of matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix().  
## If the inverse has already been calculated (and the matrix has not changed), 
## then  cacheSolve()  should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

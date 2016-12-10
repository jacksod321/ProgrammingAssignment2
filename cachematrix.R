## A funciton when the inverse matrix of input matrix x are not calculated yet
## it will calcualte, otherwise just return solved value already stored


## Define Objective like function initialize required field
## such default inverse matrix is null and what setinv and getinv will do
## and also record. the cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## If inverse of matrix x already been calculated
## return value by getinv, else will call solve function
## to calculated inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

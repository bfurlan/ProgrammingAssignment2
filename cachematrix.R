## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #set orig. matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #get orig. matrix
  get <- function() x

  #set inv matrix
  setinv <- function(i) inv <<- i
  
  #get inv matrix
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## computes the inverse of the special 
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  #if it is cashed return inv
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #otherwise calculate inv. matrix and keep it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

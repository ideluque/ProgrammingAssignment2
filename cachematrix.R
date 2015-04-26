## This function creates a special matrix "object" that can cache its inverse
### The first function, makeVector creates a special "vector", which is really a list containing a function to
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the value of the inverse of the matrix
#   4. get the value of the inverse matirx

makeCacheMatrix <- function(x = matrix()) {
  nr <- nrow(x)
  nc <- ncol(x)
  mi <- matrix(NA,nr,nc)
  set <- function(y) {
    x <<- y
  }
  get <- function() x
  setinverse <- function(solve) mi <<- solve
  getinverse <- function() mi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special matrix returned
## by the makeCacheMatrix function above. If the inverse have already been 
## calculated this function will retrieve the inverse of that matrix

cacheSolve <- function(x, ...) {
  mi <- x$getinverse()
  if(!is.na(mi[1,1])) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setinverse(mi)
  mi
}

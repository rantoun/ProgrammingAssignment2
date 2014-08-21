## Establish special "matrix" object that can cache its inverse.
## cacheSolve will compute the inverse of the special matrix object --> if inverse has already been computed, then it will retrieve the inverse from cache instead of recomputing.

## Special matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Function to compute inverse and check for existence in cache

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

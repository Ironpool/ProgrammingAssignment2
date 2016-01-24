## These functions will cache a matrix for inverse matrix calculations. 
##

## Calculates the inverse (if possible) and stores it into the cache.

makeCacheMatrix <- function(x = matrix()) {
  base = NULL
  set=function(y){
    x <<- y
    base <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) base <<- inverse
  getinv <- function() base
  list(set=set, get=get,
       setinv=setinv, 
       getinv=getinv)
}


## Calculates the inverse of the above matrix if not done so already. If already found, returns the inverse. 

cacheSolve <- function(x, ...) {
  base <- x$getinv()
  if(!is.null(base)){
    message("getting cached data")
    return(base)
  }
  data <- x$get()
  base <- solve(data, ...)
  x$setinv(base)
  base
}

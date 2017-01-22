## Functions to cache the inverse of a matrix, and recall the inverse instead of recalculating

## function that will cache a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x<<- y
    i<<- NULL
  }
  get <- function() x
  setinv <- function(inv) i<<-inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## function that will solve for the inverse of a matrix, and then cache it

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}

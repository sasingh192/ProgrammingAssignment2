## The ivnerse of matrix can be calculated using this program.
## Since matrix inversion can be a computationally expensive process,
## this program caches the inverse of matrix. This will prevent repeated
## matrix inversion calculations in case it has already been performed once.


## The first function, makeCacheMatrix creates a special "vector", which is really a list 
## containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the mean
# 4. get the value of the mean

makeCacheMatrix <-  function(x=matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setmatinv <- function(invmat) m <<- invmat
  getmatinv <- function() m
  list(set=set, get=get, setmatinv=setmatinv, getmatinv=getmatinv)
}

## The following function calculates the inverse of the inverse matrix
# using the special "vector" calculated from the above function.
# This function first checks to see if the matrix inverse has already been calculated.
# If so, it gets the matrix inverse from the cache and skips the computation. Otherwise,
# it calculates the matrix inverse of the data and sets the value of matrix inverse in the
# cache via the setmatinv function.

cacheSolve <- function(x, ...) {
  m <- x$getmatinv()
  if(!is.null(m)) {
    message("getting chached inverse of the matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setmatinv <- m
  m
}

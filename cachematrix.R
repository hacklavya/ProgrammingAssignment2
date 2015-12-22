## makeCacheMatrix function creates an object that contains 
## a matrix and its inverse and funcitons to set and get both of these.
## if inverse not calculted the a=calculaye it now and save it in ram, if it 
## already there then return the same.


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the "matrix" object created 
## by makeCacheMatrix. If the inverse already calculated then get it from the cache.

cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  mymat <- x$get()
  xinv <- solve(mymat, ...)
  x$setinv(xinv)
  xinv
}


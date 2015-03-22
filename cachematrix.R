##*****************************************************************************
##
## These functions work in conjunction to create a *special* matrix which 
## includes a built-in caching mechanism for the inverse. 
##
##*****************************************************************************


##*****************************************************************************
## Accepts a square matrix and returns it wrapped in caching functions. 
##
## The return cache matrix will be accessible via its get() function.
##*****************************************************************************
makeCacheMatrix <- function(x = matrix()) {

  ## Error Check: make sure the matrix is square
  nr = nrow(x)
  nc = ncol(x)
  if ( nr != nc ) {
    stop("Provided matrix must be square")
  }
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


##*****************************************************************************
## Computes the inverse of the provided cached matrix; if the inverse has 
## already been calculated, then the value is retrieved from a cache to enhance
## performance.
##
## The inverse matrix is available via its getInverse() function.
##*****************************************************************************
cacheSolve <- function(x, ...) {

  ## Check the cache for the inverse
  i <- x$getInverse()
  
  ## If available from the cache, simply return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Inverse has not yet been calculated, so calculate it now and store the value
  ## in the cache for all future access requests.
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i  
}

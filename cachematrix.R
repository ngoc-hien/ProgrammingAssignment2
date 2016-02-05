## This function creates a special "matrix" object that can cache its inverse.
## a function to set the value for "matrix"
## a function to get the value of "matrix"
## a function to set/get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInv <- function(invmatrix) inv <<- invmatrix
      getInv <- function() inv
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getInv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data) # calculate the inverse matrix
      x$setInv(inv)
      inv
}
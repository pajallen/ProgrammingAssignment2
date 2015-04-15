#    makechaceMatrix and cacheSolve are two functions that compute and cache the 
#    inverse of a matrix allowing retrieveal of the inverse matrix from cache if the
#    invrese of the same matrix is requested.


#     makeCacheMatrix creates a special matrix object that can cache it's inverse.
#     Returns a vector (list) used by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }  # set
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      return(list(set = set, 
                  get = get, 
                  setinv = setinv, 
                  getinv = getinv))
}  # makeCacheMatrix


#    cacheSolve produces the inverse of the matrix and uses the special vector returned by 
#    makeCacheMatrix above to determine if it needs to be computed or can be read from cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if (!is.null(inv)){
            message("Retrieving cached inverse")
            return(inv)
      }  # if (!is.null(inverse))
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      message("Computing inverse")
      return(inv)
}  # cacheSolve

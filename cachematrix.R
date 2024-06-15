
# makeCacheMatrix: This function creates a special "matrix" object that caches its inverse.
# Methods:
#    set: Assigns a new matrix and resets the cached inverse.
#    get: Returns the matrix.
#    setinv: Caches the inverse of the matrix.
#    getinv: Retrieves the cached inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  setinv <- function(y) inv <<- y
  getinv <- function() inv
    
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}


# cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated and the matrix has not changed,
# then cacheSolve should retrieve the inverse from the cache.
# If matrix is not invertible, then cacheSolve returns NaN.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("get cached inverse")
    if(any(is.nan(inv))) {
      message("Matrix is not invertible.")
    }
    return(inv)
  }
  mat <- x$get()
  if(any(is.na(mat))) {
    stop("Matrix is bad.")
  }
  if((ncol(mat) != nrow(mat)) || det(mat)==0) {
    message("Matrix is not invertible.")
    inv <- NaN
  } else {
    inv <- solve(mat, ...)
  }
  x$setinv(inv)
  inv
}
## Put comments here that give an overall description of what your
## functions do

## Creates a wrapper object for a matrix.  The wrapper is intended 
## to be used for caching a matrix inverse.
##
## @param x A matrix
##
## @return The wrapper object.
## The wrapper methods are:
## $set - set a new matrix
## $get - get the current matrix
## $setinv - set the matrix inverse
## $getinv - get the precomputed marrix inverse, or null if it has not been computed
## 
##  
makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  
  set <- function(new_x) {
    x <<- new_x
    m_inv <<- NULL
  }
  get <- function() { x }
  
  setinv <- function(new_env) { m_inv <<- new_env }
  getinv <- function() { m_inv }
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Retrieve the cached matrix inverse or compute it if it has not been
## precomputed. Inverses are computed using the solve() function
##
## @param x The matrix wrapper object created by makeCacheMatrix().
## @param ... [Optional] additional arguments to the matrix inversion function.
## @return The inverted matrix object.
cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  if (!is.null(inv)) {
    return(inv)
  }
  
  inv = solve(x$get(), ...)
  x$setinv(inv)
  
  inv
}

## Utility to test cache solve.
##
## @param m The test matrix.
## @param e The expected solution.
test_cacheSolve <- function(
  m = matrix(c(4, 7, 2, 6), 2, 2),
  e = matrix(c(0.6, -0.7, -0.2, 0.4), 2, 2)) {
  
  x = makeCacheMatrix(m)
  res1 = cacheSolve(x)
  print("Got inv:")
  print(res1)
  if (!assertthat::are_equal(e, res1)) {
    print("Failed")
    return
  }
  
  res2 = cacheSolve(x)
  if (!assertthat::are_equal(e, res2)) {
    print("Failed")
    return
  }
  print("Success!")
}

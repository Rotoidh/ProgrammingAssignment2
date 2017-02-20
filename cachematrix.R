#This is an attempt to make matrix inversion more efficient by
#caching the inverse of a matrix. It should make for a less
#costly computation by avoiding the repeated process of
#computing the inverse of a matrix.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  #initialize a null variable, will store cached value
  cachedinv <- NULL
  #set matrix
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get matrix
  getmatrix <- function() x
  #set inverse matrix
  setinverted <- function(inverse) cachedinv <<- inverse
  #get inverted matrix
  getinverted <- function() cachedinv
  #return functions matrix
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverted = setinverted, getinverted = getinverted)
}
# This function will attempt to compute the inverse of the
# matrix, returning the cached inverse if available.
cacheSolve <- function(x, ...) {
  cachedinv <- x$getinverted()
  #return cached calculation if available
  if (!is.null(cachedinv)){
    message("cached data load")
    return(cachedinv)
  }
  #no data cached, calculate
  calc <- x$getmatrix()
  cachedinv <- solve(calc,...)
  #cache inverse of new calculation
  x$setinverted(cachedinv)
  #return cached new calculation
  cachedinv
}
a <- diag(5,3)
a
CachedMatrix <- makeCacheMatrix(a)
cacheSolve(CachedMatrix)
b <- diag(2,6)
bn
b
CachedMatrix2<-makeCacheMatrix(b)
cacheSolve(CachedMatrix2)


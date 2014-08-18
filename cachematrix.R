##
## The functions makeCacheMatrix() and cacheSolve() are used to create a special object 
## that stores a square invertible matrix and caches its inverse.
## This allow one to simply look up the inverse in the cache
## and not recompute the inverse when the matrix does not change
## The functions take advantage of the scoping rules of the R language to 
## preserve state inside of an R object.
##
## makeCacheMatrix() returns a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix
##
## Input:
##      x: invertible square matrix
## Output:
##      a list of functions as described above
##
makeCacheMatrix <- function(x = numeric()) {
  
  ## initialize - cache the matrix and its inverse as a NULL value
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## return the matrix x
  get <- function() x
  
  ## cache the inverse
  setinv <- function(inv) m <<- inv
  
  ## retrieve the cached value
  getinv <- function() m
  
  ## create the list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##
## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix() 
## If the inverse has already been calculated and the matrix has not changed, 
## cacheSolve() retrieves the inverse from the cache
##
## Input:
##       x: list of matrix functions returned by makeCacheMatrix()
## Output:
##       inverse of matrix
## Notes:
##       It is assumed that the matrix is square and invertible, no error checks done
##
cacheSolve <- function(x, ...) {
  
  ## return cached inverse if computed previously
  inv <- x$getinv()
  if(!is.null(inv)) { 
    message("getting cached inverse")
    return(inv)
  }
  
  ## inverse has not been computed previously
  m <- x$get() # get matrix to inverse
  inv <- solve(m, ...) # compute inverse
  x$setinv(inv) # cache inverse
  inv # return inverse
}
##
## test function
##
testCacheMatrix <- function(x, ...){
  y <- makeCacheMatrix(x)
  cacheSolve(y)
  cacheSolve(y)
}
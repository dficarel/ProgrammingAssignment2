##  FUNCTION: makeCacheMatrix
##
##  INPUTS: x is assumed to be a square matrix which is invertible.
##  RETURNS: A list containing the following functions:
##    getmatrix(): get the matrix
##    setinverse(): set the inverse of the matrix
##    getinverse(): get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invcache = NULL
  getmatrix = function() {
#    print("In getmatrix")
    return(x)
  }
  setinverse = function(inverse) {
    invcache <<- inverse
#    print("In setinverse")
    return(invcache)
  }
  getinverse = function()  {
#    print("In getinverse")
    return(invcache)
  }
#  print("In Main Loop")
  return(list(getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse))
  }

##  FUNCTION: cacheSolve
##
##  INPUTS: x is assumed to be a square matrix which is invertible, therefore the "solve(x)" function is used to calculate the inverse.
##  RETURNS: An inverted matrix from the cache if it's already there otherwise created using the solve() function and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  elapsed <- 0
  Time1 = Sys.time()
  invcache = x$getinverse()
  if (!is.null(invcache)){
    print('The inverse of this Matrix has been found in the cache - No further computation needed!')
    Time2 = Sys.time()
    elapsed = Time2 - Time1
    cat(sprintf("In cacheSolve for: %f seconds\n", elapsed))
    return(invcache)
  }
  newmatrix = x$getmatrix()
  invcache = solve(newmatrix, ...)
  x$setinverse(invcache)
  Time2 = Sys.time()
  elapsed = Time2 - Time1
  cat(sprintf("In cacheSolve for: %f seconds\n", elapsed))
  return(invcache)
}
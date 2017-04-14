######################
######################

##The pair of functions below, when used correctly (the output of the first 
##one used as the argument to the second), return the inverse of a matrix, 
##and "cache" this result in a "matrix" object (stored in function 1), which 
##can then be retrieved when needed (using function 2), making matrix inverse
##computations much faster.
##
##The makeCacheMatrix() function takes an invertible matrix as its argument, 
##and returns a list of named elements (which are outputs from the accessor 
##and mutator methods used within the body of the function). You can then pass
##the output of thismfunction as an argument to the second function, 
##cacheSolve(), which then calculates and stores the inverse of the initial 
##input matrix in the first function, and returns this inverse as an output.
##
##Then whenever cacheSolve() is called again using the same argument (until a 
##new matrix is set using the set function), it will immediately retrieve the 
##inverse without needing to recalculate it.
##
##makeCacheMatrix(), uses two key variables, x and m; x stores the input matrix 
##data, and m stores the inverse of that matrix (though it is initialized as 0).
##
##If there is already an inverse cached in m, then the value of m cached in the 
##memory of the object is cleared whenever x is reset, forcing subsequent calls 
##to cacheSolve() to recalculate the inverse.


#This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This function computes the inverse of the "matrix" returned by makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
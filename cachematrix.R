
####CACHE INVERSE MATRIX FUNCTIONS####

##The pair of functions below, when used correctly (the output of the first 
##one used as the argument to the second), return the inverse of a matrix, 
##and "cache" this result in a "matrix" object (stored in function 1), which 
##can then be retrieved when needed (using function 2), without the need to re-
##calculate the inverse.
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
    
    #this function resets x and m when a new matrix 'y' is input: x is set to y,
    #and m is reset to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    #this function stores the value of the inputted matrix x
    get <- function() x
    
    #this function is called from cacheSolve, setting m as the newly calculated
    #inverse, and assigning it to 'setinverse'
    setinverse <- function(inv) m <<- inv
    
    #get inverse is a function which returns the value of m
    getinverse <- function() m
    
    #returns a named list, storing all the above "getter/setter" variables
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#This function computes the inverse of the "matrix" returned by makeCacheMatrix 
#function above.

cacheSolve <- function(x, ...) {
    
    #assigns to m the value of getinverse() stored in the makeCacheMatrix function
    m <- x$getinverse()
    
    #retrieves m from makeCacheMatrix function if m is not NULL
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    #otherwise assigns value of matrix x to 'data'
    data <- x$get()
    
    #then calculates and assigns the inverse of x to 'm'
    m <- solve(data, ...)
    
    #then calls the setinverse function from makeCacheMatrix() with m as input
    x$setinverse(m)
    
    #then returns 'm' to the console
    m
}
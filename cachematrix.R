## The two functions provide a means to calculate the inverse of a matrix,
## store the matrix and the inverse in a different environment,
## and provide a means to recall the inverse if it has already been calculated.

## This function takes the input matrix, x, and creates an output 
## that is comprised of a list of inputs in 
## a different environment for the cacheSolve function.  

makeCacheMatrix <- function(x) {
     m <- NULL
     set <- function (y) {
     x <<- y
     m <<- NULL  
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get, 
     setsolve = setsolve, getsolve = getsolve)

}


## This function checks the output list from the makeCacheMatrix function and 
## if the inverse has been calculated, it retrieves and prints it.  If the 
##inverse has not been calculated, it is calculated and printed.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

        m <- x$getsolve()
        if(!is.null(m)) {
              message("Getting Cached Data")
              return(m)
              m
              }
              data <- x$get()
              m <- solve(data, ...)
              x$setsolve(m)
              m

}



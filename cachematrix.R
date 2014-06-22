## R Programming Class
## Programming Assignment 2

## This program is submitted for Programming Assignment 2 of the R Programming Class (rprog-004) offered
## through Coursera.  It is located in the gitHub repository for timwood1992.
## Folder location is:  https://github.com/timwood1992/ProgrammingAssignment2


## The program creates two functions, makeCacheMatrix and cacheSolve.  These functions are used to create
## a special object that stores a matrix and cache's its inverse.  The rationale for having such a function 
## is that for a large matrix, calculating the inverse will take time to compute.  If the contents of the
## matrix have not changed, the function will look up the cached value of the inverse rather than compute
## it, saving processing time. 


## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.  The function
## creates a list of four functions which are assigned to the specified variable.  These funtions are:
##     "set" -- sets the value of the matrix
##     "get" -- gets the value of the matrix
##     "setinverse" -- sets the value of the inverse of the matrix
##     "getinverse" -- gets the value of the inverse of the matrix

makeCacheMatrix<- function(x = matrix()) {
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


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.  It
## checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setinverse function.

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

  


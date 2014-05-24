## The following two functions work together to allow caching the inverse of an
## invertable matrix

## This function returns a list of 4 functions necessary for setting a matrix 
## and caching it's inverse (if already calculated)

makeCacheMatrix <- function(x = matrix()) {
    
    #I holds the inverse
    I <- NULL
    #A function which (re)sets the matrix x, and (re)sets
    #I to NULL for later calculation
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    
    #a function to retrieve the matrix stored within x
    get <- function() x
    
    #functions to set and/or get the inverse
    setinverse <- function(inverse) I <<- inverse
    getinverse <- function() I
    
    #return these four functions as a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function checks if the inverse of the matrix has already been cached in
## I. If so, it retreves the cached inverse. If not, it inverts the matrix.

cacheSolve <- function(x, ...) {
    
    #grab whatever has been cached for I
    I <- x$getinverse()
    
    #if I was not empty, report that we grabbed the cached inverse and return 
    #that value
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    
    #otherwise, calculate inverse, cache it, and return that value
    data <- x$get()
    I <- solve(data, ...)
    x$setinverse(I)
    I
}

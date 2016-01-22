## Caching the Inverse of a Matrix

## makeCacheMatrix(x) creates a special "cahceMatrix" object that holds 
## the matrix that you pass as "x" and caches the inverse of matrix x.
## Actually cacheMatrix object is a list of functions to access stored
## matrix and cached inverse.

## Usage examples
## Create cacheMaterix object:
##     cacheMatrix <- makeCacheMatrix(rbind(c(1,2),c(2,1)))
## Access to the matrix:
##     cacheMatrix$get()
## Set new matrix:
##     cacheMatrix$set(rbind(c(1,3),c(3,1)))
## Cache inverse of matrix stored in cacheMatrix object (if it is not
## cached yet) and get the cached inverse:
##     cacheSolve(cacheMatrix)
## Access the cached inverse:
##     cacheMatrix$getsolve()

## Matrix stored in cacheMatrix object must be invertible if you call
## cacheSolve() on it, or you'll get an error.
## Cached inverse is dropped when you call the set() method


## Create cacheMatrix object from matrix "x"

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## Calculate the inverse of matrix stored in cacheMatrix object "x"
## and cache the result in "x".
## "..." args are passed to the R solve() function

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

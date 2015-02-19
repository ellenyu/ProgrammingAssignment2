## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that takes a matrix as input and then 
## return a list that contains functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m<<- inverse
        getinverse <- function() m
        list(set = set, get=get,
                setinverse = setinverse,
                getinverse = getinverse)

}


## Write a short comment describing this function
## cacheSolve is a function that takes the list output
## from makeCacheMatrix and computes the inverse of
## matrix x where y<-makeCacheMatrix(x).  If
## y<-makeCacheMatrix(x) and y$getinverse is not null then 
## cacheSolve(y) will return the cached solution instead of
## recomputing the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

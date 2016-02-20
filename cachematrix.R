## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:

## This function, makeCacheMatrix creates a special "matrix" object,
## that can cache its inverse. The function does:

## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Calculate the inverse of the matrix
## 4. Get the inverser of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function calculates the inverse of the Matrix but first checks
## if the inverse already has been calculated. If so, it gets the inverse
## from the cach and skips computation. Otherwise, it calculates the inverse
## of the data and sets the value of the inverse in the cache via the setinverse
## function.

cacheSolve <- function (x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <-solve(data, ...)
    x$setinverse(m)
    m
}

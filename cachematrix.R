## Put comments here that give an overall description of what your
## functions do
## These two function can print the supplied matrix and calculate the inverse matrix without costly calculation

## Write a short comment describing this function
## makeCacheMatrix is a function that set a matrix and its inverse matrix, and list them out
makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y){
        x <<- y
        I <<- NULL
        get <- function() x
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function() I
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    }
}  


## cacheSolve will extract inversed matrix from above function or calculate it.

cacheSolve <- function(x, ...) {
    I <- x$getinverse
    if(!is.null(I)){
        message("getting cached inverse")
        return(I)
    }
    data <- x$get()
    I <- Solve(data, ...)
    x$setinverse(I)
    I
}
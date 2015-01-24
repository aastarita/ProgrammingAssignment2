## makeCacheMatrix(x) creates a special "matrix" object
## that can cache its inverse. it returns a list
## that contains a function to:
## - set the value of the "matrix"
## - get the value of the "matrix"
## - set the value of the "matrix"
## - get the value of the "matrix"

## Example:   'x'      [,1] [,2]
##               [1,]    4    6
##               [2,]    5    7
## creates a special "matrix" object

## Precondition: 'x' is a "square" matrix that is "invertible."

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then  
## cacheSolve should retrieve the inverse from the cache. The
## function returns a matrix that is the inverse of 'x'

## Example:   'x'       [,1] [,2]
##              [1,]    4    6
##              [2,]    5    7
## cacheSolve(makeCacheMatrix(x) returns inverse of 'x':
##                      [,1] [,2]
##              [1,]    -3.5    3
##              [2,]    2.5   -2

## Precondition: 'x_list' is the special "matrix" object that is 
## returned from executing the makeCacheMatrix function.

cacheSolve <- function(x_list, ...) {
        m <- x_list$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x_list$get()
        m <- solve(data)
        x_list$setinverse(m)
        m
}

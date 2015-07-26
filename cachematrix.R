## Assignment:  Caching the Inverse of a Matrix
## This assignment is related to cache the inverse of a matrix

## makeCacheMatrix:  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## set the value of the matrix eg: x <- matrix(1:4, nrow=2, ncol=2)
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get the value of the matrix eg: m = makeCacheMatrix(x)
        get <- function() x
        ## set the value of inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        ## get the value of inverse of the matrix
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve:  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  
## If the inverse been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## Sample How to RUn

## > x <- matrix(1:4, nrow=2, ncol=2)
## > m=makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(m)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 

## The functions in this file implement a kind of "matrix" object
## which holds a square matrix and its inverse if available

## makeCacheMatrix returns a list with 4 functions 
## and 2 square matrices.
## x is the input matrix
## im is the corresponding inverse matrix or null
## set, get: set or get the input matrix
## setinverse, getinverse: set or get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) im <<- inv
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes and/or returns the inverse matrix of an
## matix object x
## if the inverse matrix is already stored in the matrix object
## it is immediately returned, else the inverse matrix is computed,
## stored in the matrix object and returned.
## an additional message is printed in case of a cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data)
    x$setinverse(im)
    im
}

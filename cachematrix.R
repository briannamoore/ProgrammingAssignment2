## This function is designed to compute the inverse of a matrix, an operation that can be very time-consuming. Furthermore, this function will allow us to store the inverse matrix for future use.

## The makeCacheMatrix function creates a “special” matrix, which allows us to: 1) set the value of the matrix; 2) get the value of the matrix; 3) set the value of the inverse; and 4) get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the matrix created with the makeCacheMatrix function. If the inverse has already been computed, it will get the inverse from the cached data. Otherwise, it will calculate and set the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

## Demonstration of how to use:
squarematrix <- makeCacheMatrix(matrix(rnorm(25),5,5))
squarematrix$get()
cacheSolve(squarematrix)
squarematrix$getinverse()



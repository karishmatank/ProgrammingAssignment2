## As matrix inversion is usually a costly computation, it may be more
## desirable to cache the inverse of a matrix instead of compute it
## repeatedly. The functions below cache the inverse of a matrix.

## The first function accepts a matrix as a parameter and creates a 
## "special" matrix, which really contains functions to 1) set the value 
## of the matrix, 2) get the value of the matrix, 3) set the value of the 
## inverse, and 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## assign null initially to m
    ## set matrix x
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x ## return x
    setInverse <- function(inverse) m <<- inverse ## cache the value of m
    getInverse <- function() m ## return m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculate the inverse of the special "matrix" created above 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    ## check if an inverse already exists in cache
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) ##compute inverse
    x$setInverse(m) ##cache inverse
    m
}

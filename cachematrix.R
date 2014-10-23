## A set of utility functions to extend the matrix object and allow it to cache
## the results of the solve function to calculate the inverse matrix.
##
## Usage:
## create a matrix,
## > m <- matrix(c(c(4,3),c(3,2)), 2, 2)
##
## use the makeCacheMatrix function to create a cacheable Matrix
## > cm <- makeCacheMatrix(m)
## > cm$get()
##       [,1] [,2]
## [1,]    4    3
## [2,]    3    2
##
## Use the cacheSolve function to
## > cacheSolve(cm)
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## This has now populated the cache, the second time the message indicates
## the cached version is used and the matrix is not recalculated.
##
## > cacheSolve(cm)
## returning cached inverse
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4


## Take a matrix and add getters and setters, plus get and set inverse.
## store the original and inverse matrices as a separate properties
makeCacheMatrix <- function(x = matrix()) {
    # initialise the inverse value to Null
    i <- NULL

    # define the set function, which stores the orginial matrix and
    # resets the inverse to Null
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    # define the get function, which returns the set matrix value
    get <- function() x

    # define the inverse setter which sets the value of our inverse matrix
    setinverse <- function(inverse) i <<- inverse

    # define the inverse getter which returns our stored inverse matrix
    getinverse <- function() i

    # return our list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return a matrix that is the insverse of x.
## If it has been previously calculated, use the previous version and don't
## recalculate it.
cacheSolve <- function(x, ...) {

    # use x's getinverse to get the cached version.
    inverse = x$getinverse()

    # if the fetched cached version is not null, say we're using the cached
    # version and return early.
    if (!is.null(inverse)) {
        # This message isn't necessary, but makes it clear we're return a cached
        # copy of the inverse.
        message("returning cached inverse")
        return(inverse)
    }

    # We didn't return, so we need to calcuate the inverse
    # Get a copy of the original matrix
    matrix <- x$get()

    # Calculate the inverse of the matrix, pass on any extra parameters we got
    # given to the solve function
    inverse <- solve(matrix, ...)

    # Set the inverse version on our cache object
    x$setinverse(inverse)

    # return the inverse version back to our requester
    inverse
}

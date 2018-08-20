## Creates a cache of special matrix for potential later
## use in the function 'cacheSolve' saving us time when
## used with large taxing matrixes

## creates a cache special matrix for later use

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setMatrix <- function(inverseM) i <<- inverseM 
    getMatrix <- function() i
    list(set = set, get = get, getMatrix = getMatrix, setMatrix = setMatrix)
}


## comuputes the actual inverse of the special matrix provide by the above function

cacheSolve <- function(x, ...) {
        i <- x$getMatrix()
        if(!is.null(i)) {
            message("getting cached matrix")
            return(i)
        }
        data <- x$get()
        i <- solve(data) 
        x$setMatrix(i)
        i
}

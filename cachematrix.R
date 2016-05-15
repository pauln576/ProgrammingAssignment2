## Caches and retrieves the inverse of a matrix.
## This allows retrieving without the need for recalculation.

## makeCacheMatric: Sets and gets the value of the matrix. Sets and gets the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinversematrix <- function(inverseMatrix) im <<- inverseMatrix
    getinversematrix <- function() im
    list(set = set, get = get,
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)
}


## cacheSolve: If matrix's inverse is known, retrieve from cache. Else, calculate and return.

cacheSolve <- function(x, ...) {
    im <- x$getinversematrix()
    if(!is.null(im)) {
            message("getting cached data")
            return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinversematrix(im)    
    im
}

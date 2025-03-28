## First, makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Create initial inverse matrix as NULL.
        inv <- NULL
        ## Store new matrix, and reset inverse.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## Return matrix.
        get <- function() x
        ## Store inverse of matrix.
        setsolve <- function(solve) inv <<- solve 
        ## Return inverse of matrix.
        getsolve <- function() inv
        
        ## Return a list of previously defined matrix and inverse.
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Then, cacheSolve computes the inverse of the special "matrix", if the inverse has already
## been calculated ante matrix has not changed, then the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Try to retrieve inverse matrix.
        inv <- x$getsolve()
        ## If found, return inverse.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## If not found, 
        ## Retrieve matrix.
        data <- x$get()
        ## Solve for inverse.
        inv <- solve(data, ...)
        ## Cache inverse.
        x$setsolve(inv)
        ## Return inverse.
        inv
}

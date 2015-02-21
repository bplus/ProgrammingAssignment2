## This is a set of two functions that will return the inverse of a matrix.
## If the supplied data has been previously cached, then the data will not be recomputed.
## Note that the code assumes that the matrix supplied is always invertible.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve 
        getinverse <- function() m
        list(set = set,
             get = get,
             getinverse = getinverse,
             setinverse = setinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        # Computing the inverse of a square matrix can be done with the `solve`
        # function in R. For example, if `X` is a square invertible matrix, then
        # `solve(X)` returns its inverse.
      
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}


## The following two functions provides to cache a matrix and its calculated inverse.

## This function defines the functions to cache and retrieve a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function returns the inverse of a cached matrix.
## It returns the cached inverse. If no inverse was cached
## it calculates it, caches it and returns the value.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i

}

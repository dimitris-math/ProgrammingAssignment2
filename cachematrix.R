
## The following function creates a matrix object, which will cache the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The following function checks if there is an already calculated inverse matrix in the cache, and if yes, it makes use of it, otherwise it calculates it from the beginning

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mtrx <- x$get()
        inv <- solve(mtrx, ...)
        x$setinv(inv)
        inv
}
 
        
}

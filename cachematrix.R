## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # A modification of the makeVector function provided
        # in the assignment instructions.
        # matrix() replaces in numeric() is the function definition
        # solve replaces mean
        # setinverse replaces setmean
        # getinverse replaces getmean
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # A modification of the cacheMean function provided 
        # in the assignment instructions.
        # getinverse replaces getmean
        # solve replaces mean to get the inverse of x
        # setinverse replaces setmean
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeMat <- function(x = matrix()) {
        inv <- NULL
        
        # This set function can actually be omitted since
        # there is no actual use for it.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Calling $get returns the matrix, x.
        # This x is set lexically when makeVector(x) was first called
        get <- function() x
        
        # The inverse of x is set into $setinv via cacheinverse()
        # by manually passing inverse into inv
        setinv <- function(inverse) inv <<- inverse
        
        # The inverse of the matrix x is stored when
        # cacheinverse is called
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# The following function calculates the inverse of the special "matrix" 
# created with the above makeMat() function. 
# e.g. how cachemean can be used: 
#       special.matrix <- makeMat(x) # where x is the matrix
#       cacheinverse(special.matrix)
# so x == special.matrix is passed into cachemean()
# and special.matrix is a list consisting of set, get, setinv and getinv
cacheinverse <- function(x, ...) {
        
        # pull out the inverse of x then....(cont. next comment)
        inv <- x$getinv()
        
        # (cont. from last comment) check whether the inverse exist.
        # if (inverse is not non-existant) ...
        # i.e. it exists... (cont. next comment)
        if(!is.null(inv)) {
                
                # (cont. from last comment) the inverse of x is returned.
                message("getting cached data")
                return(inv) 
                # and the function is broken out of
                # skipping the rest of the cacheinverse function
        }
        
        # In the absence of inverse, we retrive the matrix and solve it,
        # storing the result temporarily in inv
        data <- x$get()
        inv <- solve(data, ...)
        
        # since x is a special.matrix consisting of 
        # set, get, setinv and getinv,
        # x$setinv(inv) can be used to save inv, the inverse of x
        x$setinv(inv)
        inv
}
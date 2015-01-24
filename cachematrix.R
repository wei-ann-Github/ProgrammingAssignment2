## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeMat <- function(x = matrix()) {
        inv <- NULL
        
        # This "set" function can be used in 2 ways.
        # 1st:  mat.object <- makeMat(The.square.matrix)
        # # The argument is directly passed into x
        # OR
        # 2nd:  mat.object <- makeMat()
        #       mat.object$set(The.square.matrix)
        # # y <- The.square.matrix, then y will be passed into x
        #
        # Either 1st or 2nd method, mat.object$get() will return the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Calling mat.object$get returns the matrix, x.
        # This x is set when the set function above is executed.
        get <- function() x
        
        # The inverse of x is stored automatically 
        # into $setinv via cacheinverse(mat.object)
        setinv <- function(inverse) inv <<- inverse
        
        # To retrieve the inverse of a matrix.
        # $getinv is used in cacheinverse() as well.
        getinv <- function() inv
        
        # A list of what mat.object contains
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# The following function calculates the inverse of the mat.object
# created with the above makeMat() function.
# e.g. cacheinverse(mat.object)
# returns the inverse of the matrix in mat.object
cacheinverse <- function(x, ...) {
        
        # Pulls out the inverse of x then....(cont. next comment)
        inv <- x$getinv()
        
        # (cont. from last comment) check whether the inverse exist.
        # (!is.null(inv)) == if (inverse is not non-existant) ...
        # i.e. it exists... (cont. next comment)
        if(!is.null(inv)) {
                
                # (cont. from last comment) the inverse of x is returned.
                message("getting cached data")
                
                # the inverse is returned on the console
                # and the function is broken out of,
                # skipping the rest of cacheinverse()
                return(inv) 
        }
        
        # In the absence of inverse, we retrive the matrix and 
        # calculate the inverse using solve(),
        # storing the result temporarily in inv
        data <- x$get()
        inv <- solve(data, ...)
        
        # since x is a mat.object consisting of 
        # set, get, setinv and getinv,
        # calling x$setinv(inv) saves the inverse.
        x$setinv(inv)
        
        # The inverse is returned
        inv
}
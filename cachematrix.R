## The following functions provide the ability calculate the inverse of a matrix and 
## cache the result. If the computation is run again, it first checks to see if the
## inverse has already been calculated. If so, it leverages the value stored in the
## cache. This provides efficiency when dealing with on time-consuming computations.

## The following function creates a special "matrix." This matrix is a list containing
## functions that set and get the value of a matrix and set and get the value of the 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                set_inverse <- function(solve) i <<- solve
                get_inverse <- function() i
                list(set = set, get = get,
                     set_inverse = set_inverse,
                     get_inverse = get_inverse)

}


## The following function calculates the inverse of the special "matrix"
## created with the above function, first checking if the inverse has already
## been calculated. If so, it retrieves the inverse from the cache and
## skips the computation. If not, it calculates the inverse of matrix
## and sets the value in the cache via the `set_inverse` function.

cacheSolve <- function(x, ...) {
        i <- x$get_inverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set_inverse(i)
        i
}

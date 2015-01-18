## These 2 functions able to create an object that keeps the matrix
## and it's invert matrix, to retrive latter one from cache

## Function makeCacheMatrix takes a matrix and creates an object 
## with 4 get/set methohds to return original and inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    set_inv <- function(inv) inv_x <<- inv
    get_inv <- function() inv_x
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## creates inverted matrix, if inverted exists - takes it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv_x <- x$get_inv()
    if(!is.null(inv_x)){
        message("getting cached data")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data, ...)
    x$set_inv(inv_x)
    inv_x
}

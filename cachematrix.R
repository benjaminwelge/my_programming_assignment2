##
## The function makeCacheMatrix creates a special matrix object that caches its inverse
##

makeCacheMatrix <- function(x = matrix()) {
	
	## inv will be created and later save the value of the inversed matrix
	inv <- NULL
	
	## set & get the matrix
	set <- function(y) {
			x <<- y
			inv <<- NULL
		}
	get <- function() x
	
	## set & get the inverse of the matrix
	set_inverse <- function(inverse) inv <<- inverse
	get_inverse <- function() inv
	
	## This code returns the matrix
	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}
##
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
##

cacheSolve <- function(x, ...) {

    ## fill our value to calculate 
    value <- x$get_inverse()
	
	## now let's see if inverse is already cached and has been calculated or not,
	## if not let's get cached data and return it
    if(!is.null(value)) {
        message("getting cached data")
        return(value)
    }
	## when inverse has not been calculated yet we do so using the solve() function
	## then at the end return the calculated value
    data <- x$get()
    value <- solve(data, ...)
    x$setinverse(value)
    value
}

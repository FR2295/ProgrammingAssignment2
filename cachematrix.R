## A pair of functions that cache the inverse of a matrix
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above, 
## if the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


makeCacheMatrix <- function(x = matrix()) {
        ## initialize inv as NULL, to keep the inverse of a matrix
        inv <- NULL  
        set <- function(y) {
		## use '<<-' to define a value to assign a new x in parent env
                x <<- y
	        ## inv to instore the value of the matrix
                inv <<- NULL ##if there is a new matrix, then inv should be set as NULL
        }
        get <- function() x  ## fuction to get the value of matrix param
        setinv <- function(solve) inv <<- solve ## assigns value of inv in parent environment
        getinv <- function() inv  ## get value of inv 
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x: set as the output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        inv <- x$getinv()
		
	## if the inverse has already been computed
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
		
	## otherwise, compute the inverse by function solve
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

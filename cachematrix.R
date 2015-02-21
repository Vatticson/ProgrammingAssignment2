## These functions have the objective of computing the inverse of various matrices in an efficient way.
## In order do achieve that, already computed inverses of matrices are cached and retrieved when called.

## This function creates a new matrix (assumed to be always invertible) with a special function which
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() has two functions: 
##  It calculates the inverse of a matrix specifically created with makeCacheMatrix()
##  If that inverse is calculated, it just retrieves the inverse of that matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}

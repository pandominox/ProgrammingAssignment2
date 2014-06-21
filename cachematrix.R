# Creates object (a list) with set of functions (which are elements of the list), used to: 
# - store original object, i.e. matrix (set()) and its inverse (setinv()) in cache 
# - retrieve them from cache (get() and getinv() functions).    

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
	set <- function(y) { # set the value of the matrix
		x <<- y
		minv <<- NULL
	}
	get <- function() x # get the value of the matrix 
	setinv <- function(inv) minv <<- inv # set the value of the inverse of matrix 
	getinv <- function() minv # get the value of the inverse of matrix
	list(set = set, get = get,
			setinv = setinv,
			getinv = getinv)
}

# Calculates the inverse of matrix using solve() function and sets the value of 
# the inverse of matrix via setinv() function.
# If the inverse has already been calculated (and the matrix has not changed), 
# function skips the computations and returns the inverse of matrix
# from the cache.

cacheSolve <- function(x, ...) {
        ##Return a matrix that is the inverse of 'x'		
		minv <- x$getinv()
		if(!is.null(minv)) { # if cache exists, skips the computation
			message("getting cached data")
			return(minv) # return cached invertible matrix
		}
		# solve the inverse of matrix and return it 
		data <- x$get()
		minv <- solve(data, ...)
		x$setinv(minv)
		minv	
}

# Example use of functions
m <- matrix(c(1,2,3,4), nrow=2,ncol=2) # example matrix 2x2
mc <- makeCacheMatrix(m) # create cached matrix

cacheSolve(mc) # first use of function, there is no cached invertible matrix
cacheSolve(mc) # second use of function, cached invertible matrix is used



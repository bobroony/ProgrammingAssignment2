# When used together, these two functions will cache 
# the inverse of a matrix.
# Sample run.
# > squareMatrix <- matrix(c(2,2,3,2),nrow = 2, ncol = 2)
# > temp <- makeCacheMatrix( x = squareMatrix )
# > cacheSolve( x = temp )     # original computation
#      [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
# > cacheSolve( x = temp )     # cached answer
# getting cached data
#      [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
# > squareMatrixInverse <- cacheSolve( x = temp )
# getting cached data
# > squareMatrix %*% squareMatrixInverse     # verifying result is the inverse
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# >

# Create a special "matrix" object that can cache
# its inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL

	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m 
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Compute the inverse of the special matrix returned
# by makeCacheMatrix.  If the inverse has already been
# calculated, and the matrix has not changed, then retrieve
# the inverse from the cache
cacheSolve <- function(x) {
	m <- x$getinverse()

	if( !is.null(m) ){
		message("getting cached data")
		return(m)
	}
	
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
}

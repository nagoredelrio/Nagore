## These two functions work together to save time in computing data.
## The first one takes a matrix and stores its inverse in the cache,
## and the second one gives the inverse of the matrix by retrieving 
## it from the cache instead of doing the calculations for it.

## The makeCacheMatrix function takes a given matrix as an argument
## and returns a list with four elements that have been stored in 
## the cache (set, get, setinverse and getinverse). This list should
## be stored into a variable. This way the matrix and its inverse are
## stored in the cache.

makeCacheMatrix <- function(x = matrix()) {
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

## The cacheSolve function takes the previous list as an argument 
## and then retrieves from the cache the value of the inverse of the 
## matrix, after making sure it is stored in the cache.

cacheSolve <- function(x, ...) {
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

# These functions enables to create and use matrix wrapper that caches some
# values to speed-up calculations.

# Converts a common matrix into a new matrix object with a cache.
# Currently, the cache holds inverse of a matrix.
makeCacheMatrix <- function(m = matrix()) 
{
	this <- environment()
	matrix <- m
	inverse <- NULL
	
	# Loads a new matrix.
	set <- function(newMatrix) {
		this$matrix <- newMatrix
		this$inverse <- NULL
	}
	
	# Gets the matrix.
	get <- function() this$matrix
	
	# Returns an inverse of the matrix.
	getInverse <- function() {
		if (is.null(this$inverse))
			this$inverse <- solve(this$matrix)
		else
			message("getting cached data")
		
		inverse
	}

	# setInverse() - We don't want to expose the inverse setter; we have a lazy getter
	# that automatically calculates the inverse if it's not available yet.
	
	list(set = set,
		 get = get,
		 getInverse = getInverse)
}


# Returns the inverse of a matrix created using makeCacheMatrix().
cacheSolve <- function(matrix, ...)
{
	matrix$getInverse()
}


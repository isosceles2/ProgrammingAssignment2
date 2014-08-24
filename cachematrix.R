## The following makeCacheMatrix and cacheSolve are intended to be used
## to compute the inverse of a given matrix. Since this is time consuming,
## a cache is used to store computations. Requests requesting the same work
## will fetch the answer from the cache.

## makeCacheMatrix:
## Methods to {get, set} a cached matrix.
## Passing a matrix 'x' will set up the environment to cacheSolve that matrix.
makeCacheMatrix <- function(x = matrix())
{
	m_inverse <- NULL # the inverse martix.

        set <- function(y) # method to store the matrix
	{
		x <<- y
		m_inverse <<- NULL
	}

	get <- function() # method to fetch the matrix
	{
		x # value: the matrix
	}

        setInverse <- function(inverse) # method to store inverse matrix
	{
		m_inverse <<- inverse
	}

	getInverse <- function() # method to fetch inverse matrix
	{
		m_inverse # value: the inverse matrix
	}

	list(set = set, get = get, # return helpful list of methods available
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve:
## Given a matrix 'x', this function returns the inverse of the matrix.
## Solutions are first checked for pre-computed answers.
## If no cache answer exists, will compute and store it.
cacheSolve <- function(x, ...)
{
	# warning: although beyond the scope of this class, it is advised to
	# first verify the given matrix has an inverse. Not all do!

	m <- x$getInverse() # attempt to fetch the inverse value
	if(!is.null(m)) # determine if a value was found in cache
	{
		message("getting cached data")
		return(m) # return the cached inverse and exit function
	}

	else # no inverse was found in cache.
	{
		data <- x$get() # use the matrix from input
		m <- solve(data) # actually computes the inverse of matrix ## http://stackoverflow.com/questions/11995832/inverse-of-matrix-in-r
		x$setInverse(m) # cache the solution
		return(m) # return the solution
	}
}

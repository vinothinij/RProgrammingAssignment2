#Create a matrix object that can cache its inverse

makeCacheMatrix <- function(Mtrx = matrix()) {
        #initialize the matrix Object
        mtrxInverse <- NULL
		#set matrix
        set <- function(y) {
                Mtrx <<- y
                mtrxInverse <<- NULL
        }
		#get matrix
        get <- function() Mtrx
		#  set the inverse 
        setInverse <- function(inverse) mtrxInverse <<- inverse 
		# get function to return the cached inverse
        getInverse <- function() mtrxInverse 
		#list the available function in makeCacheMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Returns the inverse of  matrix. 
#If the inverse has already been calculated then the cachesolve will retrieve and return the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
		# check if the inverse of the matrix exists in cache
		    invMtrx <- x$getInverse()
			# if inverse of the matrix exists, then get the matrix inverse from cache and return the inverse
            if(!is.null(invMtrx)) {
			# Return the inverse from cache
                    message("getting cached data")
                    return(invMtrx)
            }
			#if the cache is empty , then proceed here
			#x$get() gets the matrix 
            data <- x$get()
			# function solve returns the inverse of the matrix 
            invMtrx <- solve(data)
			# set the inverse in cache
            x$setInverse(invMtrx)
			# return the inverse
            invMtrx
}

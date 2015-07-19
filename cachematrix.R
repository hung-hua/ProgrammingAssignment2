
## makeCacheMatrix returns an object (a list), created from a matrix.
## the object has methods (functions) that can be used to by cacheSolve to compute the inverse matrix
## get() returns the matrix from which the object was constructed
## set(x) sets the underlying matrix to the supplied matrix x
## getInverse() returns the cached inverse matrix or NULL if no value has been cached
## saveMatrix() saves the inverse matrix to the global environment
## clearSavedMatrix() sets the cached inverse matrix to NULL
## isInvertible() returns TRUE if the underlying matrix is invertible else FALSE
## typical usage
## a <- makeCacheMatrix()
## b <- matrix(runif(9,1,100),3,3)
## a$set(b)
## cacheSolve(a) - first call will cache the inverse matrix
## cacheSolve(a) - subsequent calls get cached value 
## benchmarking to prove effect of caching
## microbenchmark(cacheSolve(a, useCache = TRUE), cacheSolve(a, useCache = FALSE), times = 1000)
## Unit: microseconds
##                          expr    min     lq      mean median     uq     max neval
## cacheSolve(a, useCache = TRUE)  5.817  6.501  6.955554  7.186  7.528  13.346  1000
## cacheSolve(a, useCache = FALSE) 39.694 40.721 42.062341 41.405 41.748 174.860  1000

makeCacheMatrix <- function(x = matrix()) {
# store for the inverse matrix 
        cache <- NULL

        # create the matrix in global env
        set <- function(y) 
	{
		if(is.matrix(x))
		{
                	x <<- y
                	cache <<- NULL
		} else
		message("Error: arg supplied arg is not matrix") 
        }

        # get the value of the matrix
        get <- function() x

	# get the inverse matrix from cache
        getInverse <- function() cache

        # store specified value in cache
        saveMatrix <- function(value) cache <<- value
	
	# set cache to NULL
	clearSavedMatrix <- function() cache <<- NULL
	
	# isMatrix invertible
	# returns true if invertible else false 
	isInvertible <- function()
	{				
		out <- tryCatch( {                		
                	cache <- solve(x)
        	},
        	error = function(e) 
		{
                	message("Error:")
                	message(e)
			return(FALSE)
                
        	},
        	warning = function(e) 
		{
                	message("Warning:")
                	message(e)
			return(FALSE)		      
        	},
        	finally = {}
		 	
        	)
		if (typeof(out) == "logical")
			return(FALSE)
		else
			return(TRUE)		
	}

	# return the created functions to the working environment
        list(set = set, get = get,
             saveMatrix = saveMatrix,
	     clearSavedMatrix = clearSavedMatrix,
	     isInvertible = isInvertible,
             getInverse = getInverse)
}


## cacheSolve function returns the inverse matrix of x
## if useCache is FALSE or the cached value is NULL, the inverse of x is (re-)calculated 
## else the cached value of the inverse matrix is returned
   
cacheSolve <- function(x, ...) {

        cache <- x$getInverse()

        # return inverse matrix from cache if it exists
        if (useCache == TRUE && !is.null(cache))                       
		return(cache)
               
        # compute inverse matrix and store it in cache         
        cache <- solve(x$get(), ...)

	if(useCache == TRUE)
		x$saveMatrix(cache)                

	return(cache)
}

# regression test
# you will need to install the below package to get assert function
# install.packages("testit")
# library(testit)

cacheTest <- function()
{
	baseMatrix1 <- matrix((1:4), 2, 2)	
	a <- makeCacheMatrix(baseMatrix1)
	assert("test get", a$get() == baseMatrix1 )

	baseMatrix2 <- matrix((2:5), 2, 2)
	a$set(baseMatrix2)
	assert("test set", a$get() == baseMatrix2 )

	expectedResult <- matrix(c(-2.5,1.5,2,-1), 2, 2)
	result <- cacheSolve(a)
	assert("cacheSolve", all.equal(result, expectedResult) == TRUE)	

	result <- a$getInverse()
	assert("getInverse", all.equal(result, expectedResult) == TRUE)	

	a$clearSavedMatrix()
	result <- a$getInverse()
	assert("clearSavedMatrix", is.null(result))

	assert("isInvertible1", a$isInvertible() == TRUE)
	
	a$set(matrix((1:9), 3, 3))	
	assert("isInvertible2", a$isInvertible() == FALSE)
			
}

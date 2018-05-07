## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	## Initializing the inverse property
    i <- NULL

    ##Setting the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Getting matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Setting the inverse of the matrix
    setInverse <- function(inverse) {
        m <<- inverse
}

    ## To get the inverse of the matrix
        getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the matrix that we got from "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should get the inverse from cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from object we just created
    data <- x$get()
	
    ## Set the inverse to the object
    x$setInverse(m)


    ## Calculating the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Return the matrix
    m
}

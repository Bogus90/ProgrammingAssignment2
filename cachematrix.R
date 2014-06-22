## Functions calculate the inverse of a matrix, cache the inverse. Before the inverse of 
## a matrix is calculated, the function checks if the inverse of particular matrix is already
## stored. If yes, it retrieves it. If not, proceeds with calculations of the inverse.


## this function does the following:
## sets the value of the matrix
## gets the value of the matrix
## sets the value of the inverse
## gets the value of the inverse

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


##This function calculates the inverse of a matrix created with makeCacheMatrix.
##It checks first if the inverse has already been calculated. 
##If yes, it gets the inverse from the cache and skips the computation.
## If not, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
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

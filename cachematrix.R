## Put comments here that give an overall description of what your
## functions do
##Make the inverse matrix -solve- function cacheable. Since it is expensive to calculate the inverse of a matrix, 
##we store in the cache. 
## Write a short comment describing this function
## Make the inverse of the Matrix and store in the cache
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
            x <<- y
            inverse <<- NULL
        }
        #Define the get
        get <- function() x
        #Define the set
        setsolve <- function(solve) inverse <<- solve
        getsolve <- function() inverse
        ##Make the list and retrieve
        list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve)
}
## Write a short comment describing this function
## It retrieves the inverse if is in the cache, otherwise it calculates it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getsolve()
        ##If it is in the cache we just retrieve it
        if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        ##If not we calculate it
        data <- x$get()
        inverse <- solve(data, ...)
        x$setsolve(inverse)
        inverse
}

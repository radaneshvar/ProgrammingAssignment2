##These set of functions has been written to simplify 'inversion of matrix' and//
##//alleviate the need for repeated computation while once is previously computed//
##// and is already availabe in the cached memory.

##This function retains a 'cached version' of inverted matrix and...
##...returns the function within a list to its parent environmemnt.
makeCacheMatrix <- function(x = matrix()) {
        sol <- NULL
        set <- function(y) {
                x <<- y
                sol <<- NULL
        }
        get <- function() x
        setsolve <- function(slove) sol <<- slove
        getsolve <- function() sol
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
##This function check the memory for availabilty of inverted matrix and provide it,//
##//otherwise, it inverts the matrix and provide the results 
cacheSolve <- function(x, ...) {
        sol <- x$getsolve()
        if(!is.null(sol)) {
                message("getting cached data")
                return(sol)
        }
        data <- x$get()
        sol <- solve(data, ...)
        x$setsolve(sol)
        sol
}
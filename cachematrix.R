## This function contains a list of functions that reads and stores the 
## input (which is a matrix), and the input's inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## This function 
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## Define get function
    get <- function() x
    
    ## Define setinverse and getinverse functions
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    
    ## We need this to store the functions above
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the matrix and stores this in memory

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    ## Check if value is stored in memory, return if found
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## If value is not found, compute the inverse of the matrix 
    data <- x$get()
    i <- solve(data, ...)
    ## Save the value and return
    x$setinverse(i)
    i
}

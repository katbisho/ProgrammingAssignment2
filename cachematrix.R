## two functions that are used to create an object that creates a matrix and 
##cache's its inverse

## makeCacheMatrix creates a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

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


## the following function calculates the inverse of the special "matrix" that is
## created in the above function. 
##it first calculates to see if the inverse has already been calculated
##if it has then it skips over the computation.
## otherwise it calculates the inverse using the inverse and sets the value of 
##the inverse in the cache using the setinverse function
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

## Programming Assignment 2: Lexical Scoping
## Create functions that cache the inverse of a matrix


## Function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        x   <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(y) inv <<- y
    getinverse <- function() inv

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated then function retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    m   <- x$get()
    inv <- solve(m, ...)
    x$setinverse(inv)
    inv
}

## makeCacheMatrix is a function that acts as a blueprint to store matrix and 
## its inverse. CacheSolve function utilizes the data stored in the 
## makeCacheMatrix function to either return calculated inverse from the 
## CacheMatrix or if the inverse is null will calculate and set the inverse in 
## cache and return the inverse.

## makeCacheMatrix is a function that acts as a blueprint to store matrix and 
## its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## CacheSolve function utilizes the data stored in the makeCacheMatrix to return
## calculated inverse or set inverse if null.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (! is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

#X <- matrix(c(3, 2, 4, 16), nrow = 2)
#print(X)

#cacheOfX <- makeCacheMatrix(X)
#AI <- cacheSolve(cacheOfX)
#print(AI)

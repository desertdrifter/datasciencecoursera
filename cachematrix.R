## These functions cache the inverse of a matrix, which is 
## less computationally costly than computing the inverse repeatedly

## makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to:
## 1. Set the values of the matrix
## 2. Get the values of the matrix
## 3. Set the inverse values of the matrix
## 4. Get the inverse values of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special "matrix"
## created with makeCacheMatrix. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the mean from the 
## cache and skips the computation. Otherwise, it calculates the inverse 
## of the data and sets the inverse values in the cache via the
## setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data.")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
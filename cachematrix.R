## The following functions calculate and store the inverse matrix (i) of the
## input (x) in the parent environment of makeCacheMatrix so it does not have to
## be retrieved from the global environment.

## Stores the value of x and i. Allows cacheSolve to retrieve the value of i. 
## If x is not invertible, it stops and informs the user.

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        if (det(x) == 0) {
                stop("The matrix is not invertible")
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Retrieves i from makeCacheMatrix. If it has not been calculated yet, it
## calculates i and stores it in makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, diag(mean(dim(data))), ...)
        x$setinverse(i)
        i
}

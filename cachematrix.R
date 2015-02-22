## functions to cache matrix inverse calculation
## modeled on makeVector and cachemean examples
## USAGE: makeCMat <- makeCacheMatrix(matrix)
##		inverse <- cacheSolve(makeCMat)

## Create a special matrix, a list to set matrix, get matrix,
## set inverse and get inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## calculates inverse of special "matrix" created above.
## 1st checks if inverse has already been calculated.
## if so, retrieves from cache, else computes inv and stores in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)

        inv
}


## makeCacheMatrix is the function that can create special matrix object
## together with the cacheSolve, it can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        MatrixInv <- NULL
        set <- function(y) {
                x <<- y
                MatrixInv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {MatrixInv <<- inverse}
        getInverse <- function() {MatrixInv}
        list('set' = set, 'get' = get, 'setInverse' = setInverse, 
             'getInverse' = getInverse)
        

}


## This the cache function for the matrix
## can return a matrix that is the inverse of X

cacheSolve <- function(x, ...) {
        MatrixInv <- x$getInverse()
        if(!is.null(MatrixInv)){
                message("getting cached data")
                return(MatrixInv)
        }
        data <- x$get()
        MatrixInv <- solve(data, ...)
        x$setInverse(MatrixInv)
        MatrixInv
}

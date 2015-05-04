## The R script contains a set of functions that create and compute
## an invert form of a matrix and also cache the result.



## This function creates a special "matrix" object that can cache its inverse.

## 'x' is a matrix for which its inverse form may be computed. The matrix
##     should be invertible.
makeCacheMatrix <- function(x = matrix()) {

        matrixInversed <- NULL
        
        getMatrix <- function() {
                x
        }
        
        setMatrix <- function(newX) {
                x <<- newX
                matrixInversed <<- NULL
        }
        
        getMatrixInversed <- function() {
                matrixInversed
        }
        
        setMatrixInversed <- function(newMatrixInversed) {
                matrixInversed <<- newMatrixInversed
        }
        
        list(getMatrix = getMatrix,
             setMatrix = setMatrix,
             getMatrixInversed = getMatrixInversed,
             setMatrixInversed = setMatrixInversed)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix function. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

## 'x' is an object (list) returned by makeCacheMatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        matrixInversed <- x$getMatrixInversed()
        
        if(!is.null(matrixInversed)) {
                message("getting cached inversed matrix")
                return(matrixInversed)
        }
        
        matrixDate <- x$getMatrix()
        matrixInversed <- solve(matrixDate, ...)
        x$setMatrixInversed(matrixInversed)
        
        matrixInversed
}

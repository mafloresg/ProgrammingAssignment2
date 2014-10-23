## A pair of functions that creates a special type of matrix, calculate
## its inverse and caches it to reuse it.

## This function creates a special "matrix" object that can cache its inverse.
## theMatrix is a square matrix (NxN)
## Returns an object with the matrix and some functions used to set and get the data and the mean

makeCacheMatrix <- function(theMatrix = matrix()) {
    
    theInverse <- NULL
    
    set <- function(matrixToCalc) {
        theMatrix <<- matrixToCalc
        theInverse <<- NULL
    }
    
    get <- function() {
        theMatrix
    }
    
    setInverse <- function(meanToSet) {
        theInverse <<- meanToSet
    }
    
    getInverse <- function(){
        theInverse  
    } 
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'
## In case the inverse is already calculated, it returns the cached inverse
cacheSolve <- function(theMatrix, ...) {
    theInverse <- theMatrix$getInverse()
    if(!is.null(theInverse)) {
        message("the inverse is already calculated and the cache will be returned")
        return(theInverse)
    }
    data <- theMatrix$get()
    theInverse <- solve(data)
    theMatrix$setInverse(theInverse)
    theInverse
}

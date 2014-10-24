## A pair of functions that creates a special type of matrix, calculate
## its inverse and saves it in a "cache" in order to reuse it when is needed 
## instead of calculate it again.

makeCacheMatrix <- function(theMatrix = matrix()) {

    ## This function creates a special matrix object that can cache its inverse.
    ## theMatrix must be a square matrix (size NxN)
    ## Returns an object that contains the matrix and some functions used to set and get the data and the inverse
    
    ## set saves the original matrix and resets the inverse matrix to NULL
    ## get returns the original matrix
    ## setInverse saves the inverse matrix
    ## getInverse returns the inverse matrix
    
    theInverse <- NULL
    
    set <- function(matrixToCalc) {
        theMatrix <<- matrixToCalc
        theInverse <<- NULL
    }
    
    get <- function() {
        theMatrix
    }
    
    setInverse <- function(inverseToSave) {
        theInverse <<- inverseToSave
    }
    
    getInverse <- function(){
        theInverse  
    } 
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(theMatrix, ...) {
    ## This function returns the inverse of the matrix encapsulated in a makeCacheMatrix object
    ## In case the inverse is already calculated, it returns the cached inverse instead of calculate it again
    
    theInverse <- theMatrix$getInverse()
    if(!is.null(theInverse)) {
        message("the inverse is already calculated and the cached value will be returned")
        return(theInverse)
    }
    data <- theMatrix$get()
    theInverse <- solve(data)
    theMatrix$setInverse(theInverse)
    theInverse
}

## A pair of functions that creates a special type of matrix, calculate
## its inverse and caches it to reuse it.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(theMatrix = matrix()) {
    
    theMean <- NULL
    
    set <- function(matrixToCalc) {
        theMatrix <<- matrixToCalc
        theMean <<- NULL
    }
    
    get <- function() {
        theMatrix
    }
    
    setmean <- function(meanToSet) {
        theMean <<- meanToSet
    }
    
    getmean <- function(){
        theMean  
    } 
    
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


## Return a matrix that is the inverse of 'x'
## In case the inverse is already calculated, it returns the cached inverse
cacheSolve <- function(theMatrix, ...) {
    theMean <- theMatrix$getmean()
    if(!is.null(theMean)) {
        message("the mean is already calculated and the cache will be returned")
        return(theMean)
    }
    data <- theMatrix$get()
    theMean <- solve(data, ...)
    theMatrix$setmean(theMean)
    theMean
}

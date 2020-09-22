##Two Functions makeCacheMatrix & cacheSolve.



##makeCacheMatrix consist of set, get, setInverse, getInverse

makeCacheMatrix <- function( x= matrix()) {
    inv <- NULL         #initializing inverse as Null
    set <- function(y){                     ## function to set the Matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}   ## function to get the Matrix
    setInverse <- function (inverse) { inv <<- inverse}  ## function to set the Inverse Matrix
    getInverse <- function() {inv}    ## function to get the Inverse Matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##This function cacheSolve is used to get Cache Data.

cacheSolve <-  function (x, ...){
    inv <- x$getInverse()
    if (!is.null(inv)) {                         ## Check whether the inverse is null of not.
        message("Getting Cached Data!")
        return(inv)                           ##returns Cached Inverse
    }
    mat <- x$get()
    inv <- solve(mat, ...)           ##Calculates Inverse of Matrix
    x$setInverse(inv)              
    inv                            #Returns Inverse Matrix
} 

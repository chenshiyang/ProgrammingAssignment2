## Caching the inverse of a matrix, so that we can quickly
## get access to it as we need it. 

## This function creates a special matrix, which is really a list containing:
## 1.set the value of the matrix.
## 2.get the value of the matrix.
## 3.set the Inverse of the matrix.
## 4.get the Inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
	## the set function
    set <- function(y){
	    x <<- y
        inverse <<- NULL# if the matrix were modified, then inverse would be NULL
	}
    ## the get function
    get <- function()
        x
    ## the setInverse function
    setInverse <- function(inver)
        inverse <<- inver
    ## the getInverse function
    getInverse <- function()
        inverse
    ##return the really list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Thins function caculates the inverse matrix of the special
## "matrix" created with the makeCacheMatrix function.
## It first checks whether the inverse has already been caculated
## or not. If so, it get the inverse matrix directly. Otherwise, 
## It caculates the inverse matrix and sets the value of the inverse
## in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        inver <- x$getInverse()
        if(!is.null(inver)){# if NULL, may be uncaculated or has been changed.
            message("getting cache data")
            return (inver)
        }
        ##else, caculate it.
        matrix <- x$get()
        inver <- solve(matrix,...)
        # setInverse
        x$setInverse(inver)
        #return the inverse
        inver
}


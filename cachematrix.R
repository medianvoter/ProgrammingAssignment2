## These functions cache matrix-inverses, which saves computation resources. 
## It allows the user to first look up whether the inverse of the matrix has
## been previously calculated, and return the previously calculated inverse if
## so. Otherwise, the inverse is calculated and stored. 

## This function is the support structure for cacheSolve. It stores a matrix
## defined by the parameter x, which can be changed with set. Function get 
## returns the matrix. Function setInverse stores the inverse of the matrix, 
## which is calculated by the cacheSolve function. Function getInverse returns
## the inverse that was previously stored by setInverse. 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(newMatrix) {
        x <<- newMatrix
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function takes a makeCacheMatrix-object x and returns the inverse of x.
## If first checks if the inverse has been previously calculated. If so, it 
## returns the previously calculated inverse. If not, it calculates the inverse,
## stores it using setInverse, and then returns the inverse. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Getting cached data...")
        return(inv)
    }
    ans <- solve(x$get(), ...)
    x$setInverse(ans)
    ans
}

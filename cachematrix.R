## Matrix inversion requires costly computation, hence, caching the 
## inverse of a matrix to make it retrievable is better than repeatingly 
## compute it when needed. The two functions below are used to create 
## a special object that stores a matrix, computes and caches its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is 
## a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    setMatrix <- function(y){
        x <<- y
        inverse_x <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inv_m) inverse_x <<- inv_m
    getInverse <- function() inverse_x
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The second function, cacheSolve computes and return the inverse of the 
## "special" matrix returned by the first function, makeCacheMatrix.
## This function will first check whether inverse of the "special" matrix
## has been calculated or not, if yes, it will then return the cached inverse
## matrix, else it will computes the inverse of the "special" matrix, caches it
## and return the inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_x <- x$getInverse()
    if(!is.null(inverse_x)){
        message("getting cached inverse matrix")
        return(inverse_x)
    }
    mat_x <- x$getMatrix()
    inverse_x <- solve(mat_x)
    x$setInverse(inverse_x)
    inverse_x
}

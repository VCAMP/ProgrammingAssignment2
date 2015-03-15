## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:

## The makeCacheMatrix() function takes as input a square matrix and 
## returns a list of four functions. 
## set() and setinv() can be used to set the value of a matrix and its inverse 
## in a parent environment separate from the current environment; get()
## and getinv() can be used to return the value of the matrix and its inverse.
## The list of functions obtained through makeCacheMatrix() can be passed as 
## the input of cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        
}


## Write a short comment describing this function:

## cacheSolve takes as input a list produced by a call of makeCacheMatrix() and
## returns a matrix that is the inverse of the one passed into makeaCacheMatrix().

cacheSolve <- function(x, ...) {
        inv <- x$getinv() ##cacheSolve assigns to inv the value of the inverse
        ## matrix stored in the cache.
        
        if(!is.null(inv)){ 
                message("getting cached data")
                return(inv) ##if the inverse has already been calculated, the
                ## function will return the value stored in the cache & then
                ## terminate.
        }
        
        start_matrix <- x$get()
        inverse <- solve(start_matrix, ...)
        x$setinv(inverse)
        return(inverse) ##if the cache contains a NULL value for the inverse 
        ## of the original matrix, the function retrieves the original matrix,
        ## calculates its inverse, sets the inverse matrix value in the cache,
        ## and finally returns the inverse
}

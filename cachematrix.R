## These functions allow for the inverse to be cached and calculated.
## 

## This function allows a special "matrix" object to cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
inv<- NULL
set <- function(y) x
setInverse <- function(inverse) inv <<-inverse
getInverse <- function() inv
list(set = set,
	get = get,
	setInverse = setInverse
	getInverse = getInverse)
}


## This function calculates the inverse of the special "matrix" from the makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)) {
        	message("retrieving cached data")
        	return(inv)
        }
        data<-x$get()
        inv<-solve(data)
        x$setInverse(inv)
        inv
}
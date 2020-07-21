## Included are 2 functions: the makeCacheMatrix function creates an object for an inputted matrix and caches it and its inverse as a list.
## The 2nd cacheSolve function returns the inverse of the inputted value and checks whether its inverse is stored in the cache or will be calculated.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x<<-y
                inv <<- NULL        
        }
        get <- function() x
        setInverse <- function(inverse) (inv <<- inverse)
        getInverse <- function() inv
        list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
        }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        }

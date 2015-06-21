## the functions allow us to cache the value of the inverse so that
## when we need it again, it can be looked up in the cache rather 
## than recomputed. 

## makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## x is a square invertible matrix.

        ## this function creates a special "matrix", which is 
        ## really a list containing a function to 
        ## 1. set the value of the matrix
        ## 2. get the value of the matrix
        ## 3. set the value of the inverse
        ## 4. get the value of the inverse

        i <- NULL
        set <- function(y){
                ## "<<-" is used to assign a value to an object
                ## in an environment that is different from the 
                ## current environment. 

                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- fruntion(inverse) i <<- inverse
        getInv <- function() i
        list(set=set, get=get, setInv=setInv, 
             getInv=getInv)

        ## list is later computed by cacheSolve function 
}



## cacheSolve function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## x is a special "matrix" returned by makeCacheMatrix
        ## above.

        ## Return a matrix that is the inverse of 'x'

        i <- x$getInv
        
        ## if the inverse has already been calculated
        ## retrieve inverse from cache.

        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        ## else the function caculated the inverse of 
        ## the data 

        data <- x$get()
        i <- solve(data, ...)
        
        ## sets the value of the inverse in the cache
        ## via the setInv function.
        
        x$setInv(i)
        return(i)
}

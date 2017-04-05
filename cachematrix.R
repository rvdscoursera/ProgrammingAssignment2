## makeCacheMatrix allows for storing matrices and caching their inverse.
##
## cacheSolve caches the inverse (via the solve function) of a 
## makeCacheMatrix type object

## makes an object that contains a matrix and allows for storing a cached
## version of the matrix's inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        #Initiate inv - the cached inverted matrix as NULL. To be set later
        inv <- NULL
        
        #set - 'store' matrix
        set <- function(y = matrix()) {
                x <<- y
                #makes sure, that is matrix is re-set, inverted is cleared,
                # and has to be recalculated
                inv <<- NULL
        }
        
        # get - get matrix
        get <- function() x
        
        #setinverse - 'store' inverted matrix, when inverted is given 
        # by external function
        setinverse <- function(inverted) inv <<- inverted
        
        #getinverse - retrieve stored inverted matrix.
        getinverse <- function() inv
        
        #return list of objects and functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## sets the inverse matrix in an object of the type makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        #if inv is not null, ie. we have a cached inverted matrix
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        #if retrieved inv is null, then invert and cache
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

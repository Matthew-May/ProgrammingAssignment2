##  a pair of functions that cache the inverse of a matrix 


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    inverse<-NULL
    
    ## set
    set<-function(m) {
        x<<-m
        inverse<<-NULL
    }
    
    get<-function() x
    
    ## set inverse of matrix x
    setInverse<-function(i) inverse<<-i
    
    getInverse<-function() inverse
    
    list(set=set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## check if the inverse already exist
    inverse<-x$getInverse()
    
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    
    ## get matrix
    m=x$get()
    ## get inverse
    inverse=solve(m,...)
    ## save inverse
    x$setInverse(inverse)
    ## Return a matrix that is the inverse of 'x'
    inverse
}

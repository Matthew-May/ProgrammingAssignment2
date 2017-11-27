## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
    inverse<-NULL
    get<-function() x
    set<-function(m) x<<-m
    getI<-function() inverse
    setI<-function(i) inverse<<-i
    list(get=get,set=set,getI=getI,setI=setI)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i<-x$getI()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    m=x$get()
    i=solve(m,...)
    x$setI(i)
    i
}

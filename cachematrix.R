## The two functions are used to either retrive the cached inverse of a matrix, 
## or calculate it if the cached version is not available. The assumption
## is that the matrix is static and does not change. Also, we assume an 
## invertible matrix

## The makeCacheMatrix function is used to perform the 'set' and 'get' functions
## This means that the cached inverse is stored and retrieved using this function

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<- function(y) {
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinv<-function(invers) inv<<-invers
    getinv<-function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function takes a list as input and retrieves the inverse for the matrix
## on which that list was built. If a cached version does not exists, it
## calculates it and returns it

cacheSolve <- function(x, ...) {
    invers<-x$getinv()
    if(!is.null(invers)) {
        message("getting cached data")
        return(invers)
    }
    data<-x$get()
    invers<-solve(data)
    x$setinv(invers)
    invers   ## Return a matrix that is the inverse of 'x'
}

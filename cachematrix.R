## The following pair of functions cache the inverse of any invertible matrices. This is to avoid repeated
## computations of the inverse of the same matrix.

## The makeCacheMatrix function creates a special 'matrix' object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<- function(y){
                x<<- y
                inv<<-NULL
        }
        get<-function() x
        setinv<-function(inverse) inv<<- inverse
        getinv<-function() inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The cacheSolve function computes the inverse of the 'matrix' returned by makeCacheMatrix. It retrives 
## the inverse from the cache if the inverse was already computed. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<- x$get()
        inv<- solve(data,...)
        x$setinv(inv)
        inv
}

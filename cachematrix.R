### Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inverse_x<-NULL
set<-function(y){
  x<<-y
  inverse_x<<-NULL
}
get<-function()x
setinverse<-function(inverse) inverse_x<<- inverse
getinverse<-function()inverse_x
list(set=set, get=get,
     setinverse=setinverse,
     getinverse=getinverse)
}


## This computes the inverse of the special "matrix" returned by above.
## If the inverse has already been calculated, then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse_x<-x$getinverse()
  if(!is.null(inverse_x)){
    message("getting cached data")
    return(inverse_x)
  }
  data<-x$get()
  inverse_x<-inverse(data,...)
  x$setinverse(inverse_x)
  inverse_x
        ## Return a matrix that is the inverse of 'x'
}

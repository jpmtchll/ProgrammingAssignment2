## This file contains two functions makeCacheMatrix and cacheSolve.
## These programs will calculate an inverse of a given matrix, and cache the result for later uses.

## The below function caches a given matrix, and will cache the inverse matrix when calculated by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  #sets inverse to null, and caches initial matrix
  inv<- NULL
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  #gives the cached matrix
  get<-function() x
  #caches the just solved inverse matrix
  setinverse<-function(solve) inv <<-solve
  #gives the cached inverse matrix
  getinverse<-function()inv
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## The below function will solve the inverse of a square matrix, it it hasn't been solved yet, and then caches the 
## inverse matrix.  If it has been solved previously, the cacheSolve function returns the cached inverse matrix.

cacheSolve <- function(x, ...) {
  #sets the variable to the cached inverse matrix
  inv<-x$getinverse()
  #checks if inv is null, if not returns inv
  if(!is.null(inv)){
    message("Using cached data.")
    return(inv)
  }
  #gets cached matrix, calculates the inverse, caches inverse then returns inverse
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}

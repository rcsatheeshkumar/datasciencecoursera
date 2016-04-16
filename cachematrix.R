## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inverse<-NULL
  get<-function() x
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getInverse<-function(){
      inverse 
  }
  setInverse<-function(inv)
  {
     inverse<<- inv
  }
  list(set=set,get=get,getInverse=getInverse,setInverse=setInverse)
}


## Checks for the inverse of the matrix in cache
## if found in cache returns it or computes it and stores in cache via parent function

cacheSolve <- function(x, ...) {
      inv<-x$getInverse()
      if(!is.null(inv))
      {
        message("getting cached data")
        return(inv)
      }
      matrix<-x$get()
      inv<-solve(matrix)
      x$setInverse(inv)
      inv 
}

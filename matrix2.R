##function to cache the matrix as well as the inverse
makeCacheMatrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) { inv <<- inverse}
  getinverse <- function() {inv}
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}
##function to solve for the inverse of a matrix 
cachesolve <- function(x, ...){
  inv<- x$getinverse()
  if(!is.null(inv)){
    message("getting chache data...")       
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
  
}
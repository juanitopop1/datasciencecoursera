## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix consist of set,get,SetInv, getinv
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL           #initalizing inverse as NULL
      set <- function(y){
          x <<- y
          inv <<- NULL
      }
      get <- function() {x}   # function to get a matrix
      SetInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, SetInverse = SetInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## this is used to get the cache data
cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if(!is.null(inv)){
          message("getting cached data")
          return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$SetInverse(inv)
      inv                   ## Return a matrix that is the inverse of 'x'
}

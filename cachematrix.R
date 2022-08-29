## Put comments here that give an overall description of what your
## functions do

# makecacheMatrix creates a special matrix object that can cache or store its inverse

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


# this computes the inverse of the special matrix returned by the above function, if the inverse has already been calculated, makeCache should give the result

cacheSolve <- function(x, ...) {
        
  # returns the matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
  
}

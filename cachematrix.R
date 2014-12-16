## The two functions below are used for the purpose
## of computing the inverse of a square matrix
## and cache this inverse

## The first function creates a special vector which is
## a list containing a function to set the value of
## a matrix; get the value of a matrix; set the value
## of the inverse; get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list (set=set, get=get, setinverse=setinverse,
           getinverse=getinverse)
  
}


## The second function takes in the list returned 
## by "makeCacheMatrix" and either  calculates 
## the invese of the matrix, or takes it from cache

cacheSolve <- function(x, ...) {
  
  ## Check if inverse matris already exists      
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  ## Calculate the inverse
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  ## Return the inverse value
  inverse
}

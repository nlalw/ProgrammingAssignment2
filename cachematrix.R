## functions to set and get value of matrix and inverse matrix 
## from cache if it is present else calculate, put in memory and give values.

## function
##set the value of the Matrix
##get the value of the Matrix
##set the value of the inverse Matrix
##get the value of the inverse Matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function to get inverse of the matrix
## it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setmean(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
        
}

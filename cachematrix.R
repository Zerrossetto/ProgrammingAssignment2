## The below code implements a matrix with its cached inverse. The whole R
## script has to be intended as a proposed solution for the Coursera "R
## Programming" course

## Returns a list containing functions to set and retrieve the input matrix
## and its inverse. It follows the list of returned methods:
##  * get()         : returns the input matrix
##  * set()         : replaces che initial matrix with a new one. **NOTE**
##                    This operation deletes the cached inverse matrix, if
##                    present
##  * getinverse() : returns cached inverse matrix if present, `NULL` otherwise
##  * setinverse() : sets the value of the cached inverse matrix to a new one

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(new.matrix) {
      x <<- new.matrix
      inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse

  getinverse <- function() inv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of a matrix generated with the `makeCacheMatrix`
## function. 

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if ( !is.null(inv) ) {
    message("getting cached data")
    return (inv)
  }
  
  ## if there's no inverse in cache, solves the inverse and returns it
  x$setinverse( solve(x$get(), ...) )
  x$getinverse()
}

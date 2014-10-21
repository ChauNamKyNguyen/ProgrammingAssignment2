## Author : Chau Nguyen
## Created Date : 21st Oct, 2014
## Description : 
##      Ver 1.0 : New create
##        + makeCacheMatrix : will create a special matrix to store the matrix's invert result in cache
##        + cacheSolve : will compute the invert of a matrix if this invert doesn't exist. Else, the invert result will be
##                       restore from a cache.

## Create four functions of the matrix x.
##     + set : to set value for the matrix in x and reset the invert value of the matrix
##     + get : get the invert value of the matrix
##     + setinvert : set the invert value of a matrix in a cache
##     + getinvert : get the invert value of a matrix in a cache
## Return: return a list of four created functions  

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function () x
  
  setinvert <- function (invert) i <<- invert
  getinvert <- function () i
  
  list (set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}


## Compute the invert of a matrix if this value doesn't exist. Else, restore the invert value from the matrix's cache
## Return : Return the invest value of a "special" matrix

cacheSolve <- function(x, ...) {
  
  i <- x$getinvert ()
  
  if (!is.null(i)) {
    message ("getting cached data")
    return (i)
  }
  
  data <- x$get()
  message ("calculate the data from begining")
  i <- solve (data, ...)
  
  x$setinvert (i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}

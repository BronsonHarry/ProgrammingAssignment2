
## makeCacheMatrix is used to store and cache a matrix and its inverse
## This function creates a list containing a function to;
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse
##  get the value of the inverse

makeCacheMatrix <- function(x = matrix())  {
  ## initalise inverse as empty
  i <- NULL
  
  ## define the set function; store the matrix in global variable x
  set <- function(y) {
  x <<- y
  i <<- NULL
}
## define the get function; return the matrix
get <- function() x
## define the setinverse function; store the inverse in global variable i
setinverse <- function(inverse) i <<- inverse
## define the getinverse function; return the inverse
getinverse <- function() i
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## cacheSolve checks the cached values for the inverse matrix defined in 
## makeCacheMatrix. If present returns the cached value. If empty 
## calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## check for any cached values, if present return value
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## retrieve the matrix, find the matrix and store the inverse in the
  ## cache
  data <- x$get()  # uses get vector from makeVector function to 
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

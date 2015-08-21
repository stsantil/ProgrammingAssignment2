## This function creates a matrix objct that can cach its inverse

## makeCacheMatrix is a function that stores a list of functions
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y){  ## changes the matrix stored in the main function
    x <<- y
    m <<- NULL
  }
  get <- function() x  ## returns the matrix stored in the main function
  setsolve <- function(solve) m <<- solve 
  getsolve <- function() m 
  ##this will store the various functions
  list (set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function computes the inverse of the matrix returned by the above function. 
## If the inverse has already been calculated and there is no change in the matrix, it 
## retrieves the inverse from the cache. If not, it calculates the inverse.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)){     ## if the inverse has already been calculated, it returns it
      message("getting the inverse matrix from cache data")
      return(m)
  }
  data <- x$get()      ## if the inverse has not already been calculated, it calculates it
  m <- solve(data, ...)
  x$setsolve(m)
  m    
}

##R Script containing both the make matrix function and the cachemean function


## The first function, makeCacheMatrix a special "vect", which is a list containing a 
## function to:
## set the value of the vector
## get the value of the vector
## set the value of the Inv
## get the value of the Inv


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}



## this will return the inverse if it is allready cahced first or if not exist will calculate and return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
## makeCacheMatrix >> Creates a special object (list) with a matrix and encapsulated methods
## to compute the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {  
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## cacheSolve >> Solves the matrix calling getSolve(), caching its inverse if it is not already calculated, 
##               or retrieves its inverse, if it is already calculated
cacheSolve <- function(x, ...) {
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

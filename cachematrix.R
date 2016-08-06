## This function calculates the inverse of a squared matrix, but it the inverse has
##has been previously calculated it will display the one calculated. 

## makeCacheMatrix is a function that creates the function for getting and setting
##the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}



## This function displays the inverse (it will calculate it if inv=NULL.)

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


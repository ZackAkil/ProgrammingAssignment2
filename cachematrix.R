## makeCacheMatrix: creates a list of accessor functions for a matrix and its inverse
## that exist within a different environment.
## cacheSolve: returns the stored inverse of the matrix stored in the different 
## environment created using the makeCacheMatrix function and if it hasn't been 
## been calculated yet: calculates it and stores it using the accessor functions.

## Takes a matrix as a parameter with a default value of a empty matrix
## returns a list of getter and setter functions for the matrix and
## its inverse which use lexical scoping to access the variables declared
## within the [makeCacheMatrix] function

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y 
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Takes a special list object (produced by the makeCacheMatrix function)
## along with any extra arguments you want to pass down to the solve function
## returns the special object's inverse, if the object dosn't have a value for 
## the inverse: it will be calculated and saved inside the special object then returned 
## This function assumes that the matrix is invertible.

cacheSolve <- function(x, ...) {

  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}


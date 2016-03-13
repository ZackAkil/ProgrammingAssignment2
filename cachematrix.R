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

  s <- NULL
  set <- function(y) {
    x <<- y 
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Takes a special list object (produced by the makeCacheMatrix function)
## along with any extra arguments you want to pass down to the solve function
## returns the special object's inverse, if the object dosn't have a value for 
## the inverse: it will be calculated and saved inside the special object then return 

cacheSolve <- function(x, ...) {

  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}


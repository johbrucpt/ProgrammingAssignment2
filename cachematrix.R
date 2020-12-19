
# Assignment: Caching the Inverse of a Matrix

# Explanation of the makeCacheMatrix function:
# i <- NULL sets the inverse matrix to NULL and is a placeholder for the future inverse matrix that 
# will be calculated with cacheSolve later
# set <- function(y) {x <<- y; i <<- NULL} defines a function to set the matrix x to a new matrix y 
# and resets the inverse matrix i to NULL
# get <- function() x returns the matrix x
# setinverse <- function(inverse) i <<- inverse sets the inverse matrix i to inverse
# getmean <- function() i returns the inverse matrix
# list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) returns all the 
# defined functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Explanation of the cacheSolve function:
# i <- x$getinverse() tries to retrieve an inverse matrix 
# If i is not NULL the inverse matrix is returned to the parent environment
# If !is.null(i) is false, cacheSolve() takes the input matrix x (data <- x$get()), calculates the 
# inverse (i <- solve(data, ...)),
# uses the setinverse() function to set the inverse of the matrix (x$setinverse(i)) and then returns
# the inverse of the matrix to the parent environment at the end (i).

cacheSolve <- function(x, ...){
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

# Create a matrix to test the functions:
A <- matrix( c(5, 2, 0,
               1,-4, 6,
               2, 3,-1), nrow=3, byrow=TRUE)

# Testing the functions: 
A1 <- makeCacheMatrix(A)
cacheSolve(A1)

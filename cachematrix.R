## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix is a function capable of creating a matrix object capable ---
# of caching its inverse.

makeCacheMatrix <- function(x = matrix()) { # x argument defined as a matrix.
  
  inv <- NULL           # inv is set to NULL.
  
  set <- function(y) {  # defines the set function to assign new value ---
    x <<- y             # of matrix in parent environment.
    inv <<- NULL        # clears any value of inv that had been cached by a ---
                        # prior execution of cacheSolve().
  }
  
  get <- function() x   # defines the get function prompting argument from ---
                        # parent environment.
  
  setinverse <- function(inverse) inv <<- inverse  # assigns value of inv in ---
                                                   # parent environment.
  getinverse <- function() inv                     # gets the value of inv.
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
  # enables for the functions to be recalled with the $ operator.
  
}

## Write a short comment describing this function
# cacheSolve() computes the inverse of the matrix returned by makeCacheMatrix().
# If the inverse has been calculated, then cacheSolve() will retrieve the ---
# inverse from the cache if the matrix has not changed.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #checks to see if the inv has already been calculated. If so, the ---
  #subsequent code is not required.
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
  #If inv is NULL, it calculates the inverse of the matrix and localizes it ---
  #in the cache by the  x$setinverse(inv) function.
}

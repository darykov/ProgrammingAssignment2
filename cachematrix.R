## These two functions create a special "matrix" object (essentially a list)
# containing an a matrix (assumed to be invertible) 
# and allow to calculate and cache the inverse of the input matrix 

#This function creates a special "matrix" object that can cache its inverse.
#The fuction constructs a composite "matrix" object as a list that 
#can store a regular matrix, its inverse, and contains functions that: 
# - return the original matrix, 
# - update the original matrix, 
# - allows the inverse to be set (cached) from outside of the function, 
# - and returns the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  #Initially sets the inverse to NULL
  m <- NULL
  #Sets the original matrix equal to function input, 
  #resets the inverse to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Returns original matrix as a matrix object 
  get <- function() x
  #Sets (Caches) Inverse Matrix
  setInverse <- function(inverse) m <<- inverse
  #Returns Inverse Matrix
  getInverse <- function() m
  #Returns composite "matrix" object
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#This function computes the inverse of the composite "matrix" object 
#returned by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve 
#should retrieve the inverse from the cache
# The fuction assumes that the input matrix is invertible.

cacheSolve <- function(x, ...) {
  #Checks if the inverse is cached in the "composite" matrix object 
  #and returnes cahced inverse if it is not NULL
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #Retrieves a matrix object from composite "matrix" object, 
  #calculates its inverse and caches the inverse
  #in the composite "matrix" object
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  #Returnes the inverse matrix
  m
}

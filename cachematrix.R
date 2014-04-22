## The first function, makeCacheMatrix, is a cache object that stores a matrix and its inverse 
## The second function, cacheSolve, takes advantage of the cache by storing and retrieving 
## the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # variale that stores the value of an inverse
  inv <- NULL
  
  # function that sets the value of the matrix and nullifies teh inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # gets the matrix
  get <- function() x
  
  # sets the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # gets the inverse
  getinverse <- function() inv
  
  # prints the contents of an object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  # tries to retrieve a cached inverse value
  inv <- x$getinverse()
  
  # if the value exists in cache, it is returned
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # the value is not cached yet, so retrieve the data
  data <- x$get()
  
  # find an inverse
  inv <- solve(data, ...)
  
  # set the calculated inverse value in cache
  x$setinverse(inv)
  
  # return calculated inverse
  inv
}

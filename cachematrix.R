## The purpose of these functions is to speed up operations the require
## the inverse of a matrix. It does this by caching the value of the 
## inverse when first computed and retrieving the cached value rather
## than recomputing it when it is needed for future operations.

## This function creates an object that holds a matrix and its inverse
## The solve function is utilized in order to compute the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will retrieve the inverse from cache or compute it if NULL

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
# Check if the inverse has been calculated and cached
  if(!is.null(i)) {
    message("retrieving cached value")
    return(i)
  }

# Calculate the inverse and store it if not in cache
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

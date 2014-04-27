## makeCacheMatrix and cacheSolve are a pair of functions
## to cache inverse matrix data. 

## Function to create the caching matrix object
#- input: a matrix object
#- ouput: a list of functions that manage the state of the caching object 

makeCacheMatrix <- function(x = matrix()) {
  mat.inv <- NULL
  set <- function(y) {
    x <<- y
    mat.inv <<- NULL
  }
  get <- function() x
  setinv <- function(m.inv) mat.inv <<- m.inv
  getinv <- function() mat.inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Write a short comment describing this function
## Create a new sove function with caching functionality
#- input : caching matrix object
#- output: inverse of the matrix object

cacheSolve <- function(x, ...) {
  imat <- x$getinv() # retrieve the inverse
  if (!is.null(imat)) {
    message("getting cached data")
    return(imat)
  }
  mat <- x$get()     # extract the matrix data
  imat <- solve(mat) # calculcale the inverse 
  x$setinv(imat)     # save the inverse for future calls
  imat
}

## Example

# mat = makeCacheMatrix(matrix(c(1,0,0,2), nrow=2, ncol=2))
# imat = cacheSolve(mat)
# check: mat$get() %*% imat should print identity matrix
# any subsequent calls will use the cache

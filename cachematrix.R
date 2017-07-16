## This function creates a special "matrix" object that can cache its inverse


## set/get the value of marix
# set/get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invMatrix <<- inverse
  getinv <- function() invMatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

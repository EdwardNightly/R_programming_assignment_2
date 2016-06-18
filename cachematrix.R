## The first function produces a list of functions used to cache the inverse of the matrix provided as the argument
## the second function will check to see if the matrix has a cached inverse
## if not it will call the solve function to find the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(w) {
    x <<- w
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



# This function uses the getinverse function from 'makeCacheMatrix' to retrieve the inverse
# of the matrix if has already been calculated. If !is.null(i) is true then the inverse was already
#calculated and the function can just return the inverse. If !is.null is false, then the inverse
# was not already calculated and so the function will use the solve function to find the inverse and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}

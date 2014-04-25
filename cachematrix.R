## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## making list of setters and getters of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(invM) inv <<- invM
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  

}


## Write a short comment describing this function
## return inverse using solve() first time and caching later 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv  
}

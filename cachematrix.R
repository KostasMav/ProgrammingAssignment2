## A couple of functions usedto compute and cache the inverse
## of a matrix

## makeCacheMatrix creates a list containing functions to set and get
## a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- matrix()
      set <- function(y) {
            x <<- y
            m <<- matrix()
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## cacheSolve checks if the inverse is already calculated and returns
## its cached value if yes and computes the inverse otherwise


cacheSolve <- function(x, ...) {

      m <- x$getinv()
      if(m != matrix()) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinv(m)
      m
}

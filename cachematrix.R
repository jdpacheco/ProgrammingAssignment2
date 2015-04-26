## Due to the costly nature of the 'solve' function, and finding
## inverses of Matrices in general, to save some cycles, I have created
## a matrix which can cache its solve result

## This is the new "data structure", which is the matrix with a memory cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- null
  }
  get <- function() x
  setSolve <- function(solve) i <<- solve
  getSolve <- function() i
  list (set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## This will check to see if the solve has been stored, returning that if so, calculating new otherwise.

cacheSolve <- function(x, ...) {
        i <- x$getSolve()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setSolve(i)
        i
}

## These functions take the inverse of  a matrix and cache it.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setinverse <-function(inverse) m <<- inverse
      getinverse <- function() m
      list(set=set, get=get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The function cacheSolve takes a matrix and checks to see if the inverse
## has already been calculated. If it has already been calculated it will return
## the cache of the answer. If it has not been calculated it will calculate the inverse
## and cache the solution.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

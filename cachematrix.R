
##The first function, makeCacheMatrix creates a special "matrix",
##which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of inverse of matrix
##get the value of inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

##The following function calculates the inverse
##of the special "matrix" created with the 
##above function. However, it first checks
##to see if the inverse has already been 
##calculated. If so, it gets the inverse from
##the cache and skips the computation. 
##Otherwise, it calculates the inverse of the 
##matrix and sets the value of the inverse 
##in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        m1 <- x$get()
        m <- solve(m1,...)
        x$setinverse(m)
        m
        


}

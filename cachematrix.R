## Objective: Optimize performance for Inverse Vector by caching result

## Execution example:
## m <- makeCacheMatrix(  matrix( rnorm(16) , nrow=4 , ncol=4)  )   // cache original matrix
## cacheSolve( m )                                                  // return inverse matrix
## cacheSolve( m )           // second time, return same inverted matrix with the message "getting cached inverted vector..."

## makeCaheMatrix contains:
##                - subFunctions to get and set original matrix
##                - subFunctions to get and set inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  mInverted <- NULL
  set <- function( y ) {
      x <<- y
      mInverted <<- NULL
  }
  get <- function( ) x
  setInverted <- function( z ) {
    mInverted <<- z
  }
  getInverted <- function( ) mInverted
  list(set = set, get = get , setInverted = setInverted , getInverted = getInverted )
}



## cacheSolve retrieves
##                - cached Inverted matrix if exist
##                - or compute inverse matrix and store in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverted()
  if(!is.null(m)) {
    m <- x$getInverted()
    message("Getting cached inverse matrix !" )
    return(m)
  }
  data <- x$get()
  m <- solve(data) 
  x$setInverted(m)
  m
}
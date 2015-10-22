## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  Inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    Inv_matrix <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(invmatrix) Inv_matrix <<- invmatrix
  getinvmatrix <- function() Inv_matrix
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv_matrix <- x$getinvmatrix()
  
  if(!is.null(Inv_matrix)) {
    message("getting cached data")
    return(Inv_matrix)
  }
  data <- x$get()
  Inv_matrix <- solve(data, ...)
  x$setinvmatrix(Inv_matrix)
  Inv_matrix
}

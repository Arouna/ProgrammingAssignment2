## The following functions should be used together to reduce the computational
## time of inverting a matrix

## The first function "makeCacheMatrix" create the list and a set of functions. In object oriented 
## programming language, this function is a constructor.

## The second function "cacheSolve" is the computationnal in wich all the logic is specified



## The function below is the constructor of the list containing 4 functions: set, get, setInvMatrix and getInvMatrix

makeCacheMatrix <- function(x = matrix()) {
  
  Inv_matrix <- NULL            ## Initialize a NULL matrix
 
   set <- function(y) {         ## This function creates a matrix in the working environnement
    x <<- y
    Inv_matrix <<- NULL
  }
  get <- function() x           ## Get the value of the matrix
  
  setInvMatrix <- function(invmatrix) Inv_matrix <<- invmatrix    ## store the value of the inverted matrix in the cache
  
  getInvMatrix <- function() Inv_matrix                           ## Get the inverted matrix from the cache        
  list(set = set, get = get,                                      ## Here we construct the list containing the 4 functions
       setInvMatrix = setInvMatrix,                               
       getInvMatrix = getInvMatrix)

}


## The following function calculate the inversed matrix of list parameter x

cacheSolve <- function(x, ...) {
        
  Inv_matrix <- x$getInvMatrix()                    ## Take the inversed matrix from list member 
  
  if(!is.null(Inv_matrix)) {
    print("Extracting the inverse from the cache")    ## getting cached data if the inversed has is already computed before
    return(Inv_matrix)                              ## display the inversed matrix from the cache        
  }
  
  data <- x$get()
  Inv_matrix <- solve(data, ...)                    ## compute the inversed matrix during the first call 
  x$setInvMatrix(Inv_matrix)                        ## catching the value of the inversed matrix
  Inv_matrix                                        ## Display the inversed matrix
}

##### Below is the output of a test case. It also show how to use 
##### the functions to compute the inverse of the 3x3 matrix(c(1,4,7,2,5,13,7,45,23), 3, 3))
##### 

##  a <- makeCacheMatrix()
##  a$set(matrix(c(1,4,7,2,5,13,7,45,23), 3, 3))
##  cacheSolve(a)                           ----------- This is the first call
##             [,1]        [,2]        [,3]
##  [1,] -4.9473684  0.47368421  0.57894737
##  [2,]  2.3473684 -0.27368421 -0.17894737
##  [3,]  0.1789474  0.01052632 -0.03157895
##  cacheSolve(a)                           ------------ The Second call. We have a message below
##  [1] "Extracting the inverse from the cache"          showing that the inversed has been extracted from cache
##             [,1]        [,2]        [,3]
##  [1,] -4.9473684  0.47368421  0.57894737
##  [2,]  2.3473684 -0.27368421 -0.17894737
##  [3,]  0.1789474  0.01052632 -0.03157895
##  cacheSolve(a)                           ------------ The third call. We have a message below
##  [1] "Extracting the inverse from the cache"          showing that the inversed has been extracted from cache
##             [,1]        [,2]        [,3]
##  [1,] -4.9473684  0.47368421  0.57894737
##  [2,]  2.3473684 -0.27368421 -0.17894737
##  [3,]  0.1789474  0.01052632 -0.03157895

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The function, makeCacheMatrix creates a special list. 
## This list contains functions to
##  a. get the value of the matrix
##  b. set the value of the matrix
##  c. get the value of the mean
##  d. set the value of the mean


makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  
  get <- function() x
  
  set <- function(newx) {
    x <<- newx
    
    invx <<- NULL
    
  }
  
  getinv <- function() invx
  
  setinv <- function(newinv) invx <<- newinv
  
  
  list(get=get, set=set,getinv=getinv,setinv=setinv)

}


## Write a short comment describing this function
## This function calculates the inverse of the special "matrix" 
## created with the above function (makeCacheMatrix). 
## It first checks to see if the inverse has already been calculated with x$getinv() 
## If so, it gets the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix by using solve(data,.....) function
## sets the value of the matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getinv()
    if (!is.null(invx)) {
        message("getting cached inverse matrix")
        return(invx)
    }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
}

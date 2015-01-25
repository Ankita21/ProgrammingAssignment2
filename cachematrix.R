## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The two functions are used to calculate the inverse of matrix and if the same inverse has to
# be calculated again, the result can be taken from its cached value
# the makeCacheMatrix is called first to initialise the matrix and then the cacheSolve is called to calcualte the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                     #provides a default value equals to NULL if the cacheSolve function has not been called before 
  set <- function(y) {
    x <<- y
    inv <<- NULL                                  #provides a default value equals to NULL if the cacheSolve function has not been called before
  }                                               # set function sets the value of matrix
  get <- function() x                             # gets the value of matrix
  setinverse <- function(inverse) inv <<- inverse # sets the value of inverse
  getinverse <- function() inv                    # gets the value of inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)                   # returns a list of functions as an R object

}


## Write a short comment describing this function
#This function calculates the inverse of matrix and returns a cached value if the inverse has already been calculated before 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {                             # if the inverse has been computed before it prints the message and returns the computed cached value
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)                         # solve is used to find the inverse of the matrix
  x$setinverse(inv)
  inv       
}

## Sample run:
## > x = rbind(c(1, 2), c(-2, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00  2.00
## [2,] -2.00  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,]      0.2     -0.4
## [2,]      0.4      0.2

## In the second run, the value is retrieved from the cache
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,]     0.2       -0.4
## [2,]     0.4        0.2

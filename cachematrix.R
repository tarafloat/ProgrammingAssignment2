## My solution to R Programming Assignment 2 finished on March 22, 2015
## Functoin Objective: This R function that is able to cache the inverse of a matrix.
##
## Function usuage example:
## invt <- makeCacheMatrix(matrix(1:4, nrow=2))
## all(invt$get() %*% cacheSolve(invt) == diag(2)) # return TRUE
## all(invt$get() %*% cacheSolve(invt) == diag(2)) # print "getting cached data" and return TRUE


## The function "makeCacheMatrix" returns a list containing functoin to:
## 1. set the matrix value
## 2. get the matrix value
## 3. set the inverse value of the matrix
## 4. get the inverse value of the matrix
makeCacheMatrix <- function(x = matrix()) {
# set the initial value of the inverse to NULL
invt <- NULL
## four interface methods are set as follow:
#  1.set a matrix value
set <- function(y) {
# replace old x with new value y
x <<- y
# reset stored cache
invt <<- NULL
}
#  2.get matrix value
get <- function() x
#  3.set the inverse value of the matrix
setinverse <- function(solve) invt <<- solve
#  4.get the inverse value of the matrix
getinverse <- function() invt
# list of all the functions
list(set = set, get = get,
setinverse = setinverse,
getinverse = getinverse)
}

## This function computes the inverse matrix returned by makeCacheMatrix above.
## If the inverse of the same matrix has not been cached,
## it computes it and caches the value.
## Otherwise it retrieve the inverse from the cache and returns the cached inverse.
## The required parameter is the list with interface functions
cacheSolve <- function(x, ...) {
# Read the inverse matrix
invt <- x$getinverse()
# If the inverse is cached, return its value
if(!is.null(invt)) {
message("getting cached data")
return(invt)
}
data <- x$get()
# Otherwise compute the inverse matrix, cached and returned it
invt <- solve(data, ...)
# data which we store in cachable matrix
x$setinverse(invt)
# Return the inverse matrix of 'x'
invt

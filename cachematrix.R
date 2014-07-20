## 
## This is my solution for programming assignment#2 of the course R Programming
## run by coursera.org and instructed by Johns Hopkins University staff.
##
##  Author: Sæmundur Melstað
##  Date..: 20.7.2014
##
## Following functions calculates an inverse of a matrix(x) and cache's the outcome so next
## time the inverse is calculated the value is retrieved from the cache instead of making 
## the calculation again. This saves significant time if the matrix is large.

## Function makeCacheMatrix
## ------------------------
## This function takes as a input a matrix x and creates a object cacheMatrix 
## for caching the inverse of matrix x.
## 
##  x   : stores the matrix x
##  inv : stores the inverse of matrix x in the object
##  set : is a method to store the matrix x in the object
##  get : is a method to retrieve the matrix x from the object
##  setinv : is a method to store the inverse of matrix x in the object
##  getinv : is a method to retrieve the inverse of matrix x from the object

##  Example of usage: 
##  -----------------
##  my_matrix <- matrix(c(1,2,3,4),2,2)
##  my_cacheMatrix <- makeCacheMatrix(my_matrix)
##
##  > my_cachedMatrix$get()
##       [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Function cacheSolve
## ------------------------
## This function takes as a input a object x of type cacheMatrix and calculates the inverse 
## of the matrix which is stored inside the object. If the inverse has been calculated 
## previously the result is retrieved from the object instead of calculating it again.

##  Example of usage:
##  -----------------
##  > cacheSolve(my_cachedMatrix)
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##
##  > cacheSolve(cachedMatrix)
##  getting cached data
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}

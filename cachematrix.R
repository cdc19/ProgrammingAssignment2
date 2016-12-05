## Testing 'makeVector' and 'cachemean' to learn how to create 'makeCacheMatrix' and 'cacheMatrix'.

makeVector <- function(x = numeric()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setmean <- function(mean) m <<- mean
     getmean <- function() m
     list(set = set, get = get,
          setmean = setmean,
          getmean = getmean)
}

cachemean <- function(x, ...) {
     m <- x$getmean()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- mean(data, ...)
     x$setmean(m)
     m
}

## The purpose of these functions are to create a matrix, calculate the inverse of the matrix, and cache the value of the 
## inverse for later use.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 
##   1. set the value of the vector
##   2. get the value of the vector
##   3. set the value of the mean
##   4. get the value of the mean

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

## This function computes the inverse of the special "matrix" returned by 'makeCacheMatrix' above. 
## If the inverse has already been calculated (and the matrix has not changed), then 'cacheSolve' should retrieve the 
## inverse from the cache.
## Note: This only works if the matrix is a square, inversable matrix.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setinv(inv)
     inv
}

## Testing makeVector
## x <- makeVector(1:1000)
## cachemean(x)
## cachemean(x)

## Testing makeCacheMatrix
## set.seed(1110201)
## r = rnorm(1000000)
## x = matrix(r, nrow=1000, ncol=1000)
## solve(x)
## CM <- makeCacheMatrix(x)
## CM$get()
## cacheSolve(CM)
 
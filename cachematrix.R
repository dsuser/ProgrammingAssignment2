## The following functions will enable caching the inverse of a matrix
## to avoid recomputing it repeatedly
##
## The makeCacheMatrix function creates a special "vector" which is a list
## containing functions to 1) set the value of matrix, 2) get the value of matrix,
## 3) set the value of Inverse of a matrix, 4) get the value of inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  InvMatrix <- NULL
  set <- function(y) {
    x <<- y
    InvMatrix <<- NULL
}
get <- function() x
setinverse <- function(solve) InvMatrix <<- solve
getinverse <- function() InvMatrix
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the special "vector" created
## by the above function. It first checks if the inverse is already calculated, if 
## it gets the inverse from the cache otherwise it calculates the inverse using solve
## function and sets the value of inverse in cache using setinverse function
cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  InvMatrix <- x$getinverse()
  if(!is.null(InvMatrix)){
      message("inverse is cached. Skipping computation and retrieving from cache")
      return(InvMatrix)
  }
  message("inverse is not cached. computing inverse")
  matrixdata <- x$get()
  InvMatrix <- solve(matrixdata, ...)
  x$setinverse(InvMatrix)
  InvMatrix
}


## Results from testing the function execution
##> A=rbind(c(1, -1/4), c(-1/4, 1))
##> A
##[,1]  [,2]
##[1,]  1.00 -0.25
##[2,] -0.25  1.00
##> solve(A) %*% A
##[,1] [,2]
##[1,]    1    0
##[2,]    0    1
##> matrixA <- makeCacheMatrix()
##> matrixA$set(A)
##> cacheSolve(matrixA)
##inverse is not cached. computing inverse
##[,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
##> cacheSolve(matrixA) %*% A
##inverse is cached. Skipping computation and retrieving from cache
##[,1] [,2]
##[1,]    1    0
##[2,]    0    1
##> 
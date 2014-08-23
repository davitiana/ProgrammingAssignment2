## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
   # initialize i which holds the value of the inverse of a matrix
   i <- NULL
   
   # set is used to assign a value to x outisde current environment
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   
   # get is used to retrieve x
   get <- function() {x}
   
   # setinverse is used to assign the inverse value to i
   setinverse <- function(inverse) {i <<- inverse}
   
   # getinverse is used to retrieve i, which holds the inverse value
   getinverse <- function() {i}
   
   # special vector of functions
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache
cacheSolve <- function(x, ...) 
{
   # get the value of x and copy into i, which holds the value of inverse of matrix x
   i <- x$getinverse()
   
   # if we have already calculated the inverse i, retrieve it from cache
   if(!is.null(i)) 
   {
      message("getting cached data")
      return(i)
   }
   
   # since we don't have the inverse of x in cache, calculate its value
   data <- x$get()
   i <- solve(data, ...)
   
   # store the inverse value 
   x$setinverse(i)
   
   # return i
   i
}

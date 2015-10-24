## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {
   s <- NULL
   set <- function(y) {
        x <<- y
        s <<- NULL
      }
   get <- function() x
   setsolve <- function(solve) s <<- solve  ## creates the inverse of the matrix and stores in setsolve
   getsolve <- function() s                 ## gets the inverse
   list (set = set, get = get,              ## creates the list of the matrix and it's inverse
           setsolve = setsolve,
           getsolve = getsolve)
 }
##  cacheSolve: This function computes the inverse of the special "matrix" returned by cacheSolve
## this function will search the cache data and if it does not find, then it will calculate inverse and store 
## into the list

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
       s <- x$getsolve()
       if(!is.null(s)) {
         message("getting cached data")
         return(s)
         }
       data <- x$get()
       s <- solve(data, ...)
       x$setsolve(s)
       s
       }
        


#Matrix inversion is usually a costly computation and there may be some benefit to caching the 
#inverse of a matrix rather than compute it repeatedly. Here are a pair of functions 
#that cache the inverse of a matrix.

#Note that these functions will only work if the matrix supplied is invertible. If it is not, 
#an error will be returned, e.g.:
#Error in solve.default(data, ...) : 
#Lapack routine dgesv: system is exactly singular

#To use:
#a <- matrix (1:4, 2,2)
#b <- makeCacheMatrix(a)
#cacheSolve(b)

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x) {
      #make empty matrix
      m <- matrix(NA,nrow(x),ncol(x))
      #create set function that sets values
      set <- function(y) {
            x <<- y
            #make empty matrix
            m <<- matrix(NA,nrow(x),ncol(x))
      }
      #create get function that gets values
      get <- function() x
      #setsolve takes the solution "solve" and sets it in the cache
      setsolve <- function(solve) m <<- solve
      #getsolve gets a solution from the cache
      getsolve <- function() m
      #print out list of functions for debugging
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated and the matrix has not changed, then cacheSolve retrieves 
#the inverse from the cache.
cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      #if there is a stored inverse already, get the cached inverse
      if(!(any(is.na(m)))) {
            message("getting cached data")
            return(m)
      }
      #otherwise, use solve() to get the inverse for the first time, then set the solution in the cache
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
## Programming Assignment Week 2 CWarner
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {   ##Define Arg, default Matrix
      getinv <- NULL            ##Set variable as null for hold
      set    <- function(y) {   ##Set value of matrix in parent environment
        x<<- y
        getinv <<- NULL         ##Set getinv variable null in parent
      }
      get <- function() x      ##define the get function return matrix
      setinverse <- function(inverse) getinv<<-inverse ##assugn value of inv in parent
      getinverse<- function() getinv      ##get value of getinv where cxalled
      list(set = set , get= get, setinverse= setinverse, getinverse=getinverse)
}     ##the above defines alias to call with $


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
      setinv <- x$getinverse()             ## if inverse has been solved
      if(!is.null(setinv))    {            ##if not null then
        message("getting cached data")     ##    else below
        return(setinv)  
      }
      data<- x$get()
      setinv<- solve(data,...)             ## solves for inverse
      x$setinverse(setinv)
      setinv
}

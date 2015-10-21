## Put comments here that give an overall description of what your
## functions do

## This function creates an array of matrixes

makeCacheMatrix <- function(x = matrix()) {       
     ## defines m here, first so this is the primary m variable in this function
     ## the m's used later in the below interal functions need to use --> to write
     ## to this variable
     m <- NULL
     ## the internal set function sets matrix and clears the m variable
     ## which is a flag that the inverse has not been calculated yet
     set <- function(y) {
     x <<- y
     m <<- NULL
     }
     ## the internal get function returns the previously set matrux
     get <- function() x
     ## the internal setinverse function passes the externally calculated inverse 
     ## (inv variable) and sets the m variable to it
     setinverse <- function(inv) m <<- inv
     ## the internal getinverse function returns the m variable, it will
     ## return NULL if the inverse has not been set and it will return the
     ## inverse if it has been set
     getinverse <- function() m
     ## calling the function will list the result of all of the internal functions
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function returns the cached inverse and notifies the user if it is
## returning a cached value

cacheSolve <- function(x, ...) {
     ## call the getinverse property of the input object
     m <- x$getinverse() 
     ## if the getinverse property returns !NULL, then the inverse has been set
     ## notify the user that they are getting a cached value
     ## the return command ends the function flow if the inverse has been cached
     ## and returrns the cached inverse
     if(!is.null(m)) {
          message("getting cached data")
          return(m) 
     }
     ## the funciton continues to here ONLY if the invers has not been cached
     ## import a local copy of the matrix
     data <- x$get()
     ## calculate the local copy of the inverse
     m <- solve(data, ...)
     ## call the setinverse method of the object, which sets that functions m variable
     ## to the local m variable (which is the inverse calculated here)
     x$setinverse(m)
     ## return the local m variable
     m
        
}

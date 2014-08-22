## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 
## The pair of functions below cache the inverse of a matrix so it is not recalculated each time for the same matrix.

################################################################################################
## makeCacheMatrix:
## This function returns a special "matrix" object that can cache its inverse.
## The special matrix object is really a list of functions executable on that matrix
################################################################################################
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL    #  i will be the 'inverse' and it is reset to NULL every 
               #  time makeCacheMatrix is called
  
  set <- function(y) { ## "set" is not called at all in this script. It can be called independently after  
    x <<- y			   ## a call to "makeCacheMatrix" to override original x matrix input via superassignment
    i <<- NULL		   ## if/when "set" is called it will also reset the inverse 'i' to NULL
  }
  
  #  note these next three functions are not run when makeCacheMatrix is called.
  #   instead, they will be used by cachesolve() to get values for x or for
  #   i (inverse) and for setting the inverse
  
  get <- function() { x }   # this function returns the value of the original matrix
  
  
  setinverse <- function(inverse){  # setinverse is called by cacheSolve() during the first cacheinverse()
    i <<- inverse                   # access and it will store the value using superassignment
  }                
  
  getinverse <- function(){    # getinverse will return the cached value to cacheSolve() on subsequent (non-first) accesses
    i
  }   
  
  list(set = set, get = get,      #  This list is returned with the newly created object.       
       setinverse = setinverse,    #   It lists all the functions ("methods") that are part of
       getinverse = getinverse)    #   the object.  If a function is not on the list then it cannot
                                   #   be accessed externally.
}


######################################################################################################
## cacheSolve:
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache.
######################################################################################################

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  if(!is.null(i)) {             ## this condition will be met on subsequent (non-first) runs of cacheSolve for a given matrix
    message("Getting cached inverse.")
    return(i)
  }
  data <- x$get()              ## access the original matrix using its function "get"
  i <- solve(data, ...)        ## calculate the inverse of original matrix using inbuilt R function "solve"
  x$setinverse(i)              ## set inverse of original matrix to "i" calculated above
  i
}

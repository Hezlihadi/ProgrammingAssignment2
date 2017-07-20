## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can 
## cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL             ## the value of inv is undefined 
      set <- function(y){     ## this set the value of the matrix 
            x <<- y
            inv <<- NULL      
            
      }
      
      get <- function() x 
      Invset <- function(inverse) inv <<- inverse  ##check to see if the inverse 
      Invget <- function()inv                      ##has already been calculated
      list(set = set,
           get = get,
           Invset = Invset,
           Invget = Invget)
      
}


## This function computues the inverse of the special "matrix"
## returned by makeCachematrix above.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$Invget()
      if(!ls.null(inv)) {
              message("getting cached data")
              return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$Invset(inv)
      inv
        ## Return a matrix that is the inverse of 'x'
}

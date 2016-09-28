## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  
    inv = NULL  # setting up an empty object to fill as the inverse matrix
    set= function(y){
    x <<- y   ## x is cached as y
    inv <<- NULL  #inverse is cached as null until filled.
  }
   get = function() x  # getting matrix
   setInv = function(inverse) inv <<- inverse ## setting inverse matrix and caching as object "inverse"
   getInv <- function()inv  ## getting inverse matrix
   list(set = set, get = get, setInv = setInv, getInv = getInv) ## to be supplied to cacheSolve
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {     ## supply the matrix cached from above
      inv <- x$getInv()  ## assigning either the null object or the calculated matrix
      
      if(!is.null(inv)){
        
        message("getting cached data") ## message to print if the object "inv" has a value that is not null.
        
       return(inv)  ## print to console the inverse matrix
        
    }
    mat.data <- x$get()  ## assignment of matrix from the above function for calculation of matrix inverse
    
   inv <- solve(mat.data, ...)  ## calculating the inverse of the matrix(conditional upon "inv" being null)
   
    x$setInv(inv) ## set the inverse matrix and cache as object "inverse"
    
    return(inv)  ## print to console the inverse matrix
}

## test
num <- 1:4
mat <- matrix(num, nrow =2, ncol =2) ## just creating a small matrix that I could calc an inverse by hand


value <-makeCacheMatrix(mat) ## test on matrix. 
cacheSolve(value) ## apply the retrieval of cache value or calculation of inverse matrix
##New inverse matrix should be (by row) (-2, 1.5) and (1, -.5)
## call again to test printing of message
## it worked!

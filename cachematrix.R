## Create cache matrix functions

## Cache matrix object that you can set and get matrix 
## and get and set inversion of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(invert){
    inv <<- invert
  }
  getInv <- function() inv
  
  list( set = set, get = get,setInv = setInv, getInv = getInv)
}


## calculate inversion of matrix x
## by checking if x has inversion in cache
## It will return cache, if not it will 
## calculate new inversion of matrix x

cacheSolve <- function(x, ...) {
  
  inv <- x$getInv()
  
  ##If x has cached inversion
  if(!is.null(inv)){
    message("getting cached inverse of matriz")
	## Return a cached matrix that is the inverse of 'x'
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
  
}

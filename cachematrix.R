## Create cache matrix

## Create cache matrix

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


## Invert matrix x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  
  if(!is.null(inv)){
    message("getting cached inverse of matriz")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
  
}

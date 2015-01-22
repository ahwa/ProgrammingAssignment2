## Put comments here that give an overall description of what your
## functions do

## initialize the data and generate a list to set/get data and set/get inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(inverse) inv <<- inverse
  
  getInv <- function() inv
  
  list( set = set, get = get,
        setInv = setInv, getInv = getInv )
}


## get the cached inverse and compute the inverse if the cache is NULL

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInv()
  ## check and return cached inverse if it is not NULL
  if(!is.null(inverse)) {
    message( "getting cached data")
    return(inverse)
  }
  ##compute inverse from data
  data <- x$get()
  inverse <- solve(data)
  x$setInv(inverse)
  inverse
}

## The two functions below help to create a special matrix which can cache its 
## inverse and return it when everit is needed without re-calculating it

## This function accepts a matrix and returns a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  ## Save inverse to cache
  setinverse <- function(value) inverse <<- value
  ## Return inverse
  getinverse <- function() inverse
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## This function returns the cached inverse of the special matrix 
## created by the makeCacheMatrix method. 
#It computes the inverse and caches it, if it does not already exist

cacheSolve <- function(x, ...) {  
  
    ##get cached inverse")
    inverse <- x$getinverse()
    ## check if inverse is not null and return it
    if(!is.null(inverse))
    {
      message("getting cached inverse")
      return(inverse)
    }
    ## Get matrix and compute inverse
    data <- x$get()
    inverse <- solve(data)
    ## Save inverse to cache
    x$setinverse(inverse)
    
    inverse
}

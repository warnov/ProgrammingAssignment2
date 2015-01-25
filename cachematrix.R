##Matrix inversion is usually a costly computation 
#and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly 
## These pair of functions cache the inverse of a matrix.

##You first have to create a CacheMatrix object usting the makeCacheMatrix object
##Then you pass this object to the function cacheSolve, which will operate in 
##an optimal approach the inversion of the given matrix. Yhis is,
##if the inverse was calculated before, it wont be calculated but recovered
##from the cache> otherwise, it will be calculated just one time.

##This functions assume the work with inversible squared matrices.

##Using sample:
##      > m1 <- makeCacheMatrix(mat)
##      > cacheSolve(m1)

## This function creates a special "Cache matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #We Will Storage the value of the inverse in this variable
  minv <- NULL
  set <- function(y){
    
    ##the <<- operator an be used to assign a value to an object 
    ##in an environment that is different from the current environment.
    x <<- y 
    ##If the content changes, then the invers is no longer valid, so it is deleted
    minv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse)minv<<-inverse
  getinv <- function()minv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'  
  
  ##We recover the current value from the inverse
  minv <- x$getinv()
  if(!is.null(minv)){
    ##The value was calculated before, so let's print it and finish the function
    message("getting cached data")
    return(minv)
  }
  ##The value wasn't calculated before, so let's calculate it
  data <- x$get()
  minv <- solve(data,...)
  x$setinv(minv)
  ##And print it
  ##Observe no caching message is emmited this time
  minv
}

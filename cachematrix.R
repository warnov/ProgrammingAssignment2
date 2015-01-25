##Matrix inversion is usually a costly computation 
#and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly 
## These pair of functions cache the inverse of a matrix.

##You first have to create a CacheMatrix object usting the makeCacheMatrix functions
##Then you pass this object to the function cacheSolve, which will operate in 
##an optimal approach the inversion of the given matrix. This is,
##if the inverse was calculated before, it won't be calculated but recovered
##from the cache; otherwise, it will be calculated just one time.

##These functions assume the use of inversible squared matrices.

##Using sample:
##==============================================

##  		> mat <- matrix(3:6,2)
##            [,1]  [,2]
##      [1,]    3    5
##      [2,]    4    6
##			> cachingMat <- makeCacheMatrix(mat)
##			> cacheSolve(cachingMat)
##					  [,1] 	  [,2]
##			[1,]   -3  		2.5
##			[2,]    2 		-1.5
##      Let's try again (now the value should be retrieved from cache):
##			> cacheSolve(cachingMat)
##			getting cached data       ---> Observe we are getting the data from cache
##					  [,1]  	[,2]
##			[1,]   -3  		2.5
##			[2,]    2 		-1.5
##			> 

## This function creates a special "Cache matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #We Will Storage the value of the inverse in this variable
  minv <- NULL
  
  ##Used to modify the data of the matrix
  set <- function(y){
    
    ##the <<- operator an be used to assign a value to an object 
    ##in an environment that is different from the current environment.
    x <<- y 
    ##If the content changes, then the inverse is no longer valid, so it is deleted
    minv <<- NULL
  }
  ##Getting the data from the matrix
  get <- function()x
  ##Storing the inverse matrix in the cache
  setinv <- function(inverse)minv<<-inverse
  ##Retrieving the inverse matrix from the cache
  getinv <- function()minv
  ##Documenting the functionality of these object on the screen
  ##When the function is called
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'  
  
  ##We recover the current value from the inverse stored in the cache
  minv <- x$getinv()
  if(!is.null(minv)){
    ##The value was calculated before, so let's print it and finish the function
    message("getting cached data")
    return(minv)
  }
  ##The value wasn't calculated before, so let's calculate it
  ##Retrieve the original matrix
  data <- x$get()
  ##Calculate the inverse
  minv <- solve(data,...)
  ##Store the inverse in the cache
  x$setinv(minv)
  ##And print it
  ##Observe no caching message is emmited this time
  minv
}

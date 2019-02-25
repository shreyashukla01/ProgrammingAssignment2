## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is used to cache the inverse of a matrix.
##      It takes a matrix as input and initializes 
##      an object with functions for getting and setting
##      the inverse of that matrix. Matrix is assumed to be 
##      a square matrix.
## example: 
## myMatrix <- makeCacheMatrix(matrix(runif(4, 1, 2), ncol = 2, nrow = 2))

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
  	set<-function(y){
   	 	x<<-y
    		inv<<-NULL
  	}
  	get<-function() x
 	setinverse<-function(inverse)inv<<-inverse
  	getinverse<-function()inv
  	list(set=set, get=get, 
      setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve takes a makeCacheMatrix object and checks to see if
##      the inverse matrix has already been calculated and cached.
##      If cached, the cache is returned. Otherwise, the inverse matrix 
##      is calculated and cached for subsequent execution.
## example:
## cacheSolve(myMatrix) # First time returns calculated inverse matrix
## cacheSolve(myMatrix) # Second time returns cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
  	if(!is.null(inv)) {
    		message("getting cached data")
    		return(inv)
  	}
  	data <- x$get()
  	inv <- solve(data)
  	x$setinverse(inv)
  	inv
}

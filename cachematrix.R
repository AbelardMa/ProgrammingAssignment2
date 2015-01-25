## These two functions will cache the inverse of a matrix.

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL  	##Setting m to NULL, so that in effect m would be created in this function and R would not search for m elsewhere in 'higher' environments.
  set<-function(y){
    x<<-y   ##Took input and assigned it to x in the parent environment of the function.
    m<<-NULL
  }
  get<-function() x   ##returned 'x' from its parent environment. 
  setinverse<-function(solve)m<<-solve
  getinverse<-function()m		##Assigning the local m to getinverse.
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## Return a matrix that is the inverse of 'x'. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){							##The variable m from the parent environment of the 'makeCacheMatrix' function is assigned the value of the mean using the 'getinverse' method of the makeCacheMatrix function.
    message("getting cached data")
    return(m)
  }
  data<-x$get()		
  m<-solve(data,...)
  x$setinverse(m)  ##Passing the inverse to the setinverse method of the makeCacheMatrix function. Assigning the inverse to the 'm' variable in the parent environment of the makeCacheMatrix function.
  m
}

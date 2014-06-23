## makeCacheMatrix and cacheSolve functions are used to cache the inverse
## of a matrix, which mighy be otherwise computational time consuming. 


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      ## Inverse matrixe cached
      inv<-NULL
      
      ## Set function take a input matrix and cached it
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      
      ## Get function retrieves the matrix that is previously cached 
      get<-function() x
      
      ## Setinv function take a input matrix and cached it as the inverse of
      ## the cached matrix
      setinv<-function(inverse) inv<<-inverse
      
      ## Getinv function retrieves the cached inversed matrix
      getinv<-function() inv
      
      
      list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      ## Retrieve inverse matrix cached in x
      inv <- x$getinv()
      
      ## Check whether inverse matrix exist or not. If exist, return the
      ## inverse matrix cached in x
      if(!is.null(inv)) {
            message("getting cached inversed matrix")
            return(inv)
      }
      
      ## If inverse matrix does not exist, read matrix cached in x, and 
      ## calculate its inverse matrixf
      data <- x$get()
      
      ## Calculating inverse matrix
      inv <- solve(data, ...)
      
      ## Store the inverse matrix after computating
      x$setinv(inv)
      
      ## Return inverse matrix
      inv
}
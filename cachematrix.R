
## >Purpose is to cache the reuslts of the 2  2 functions decribed below 
##in order to avoid repeating the same variable calculation thus saving computig time

##1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


##original matrix is "M"; inverted matrix is "IM"

makeCacheMatrix <- function(M = matrix()) {
  IM<-NULL
  
## "<<-" operator assigns a value to the free variable "v" to the "cachesolve" function instead of the GLOBAL environment
  set<-function(v){
    M<<-v
    IM<<-NULL
  }
  get<-function() M
  setmatrix<-function(solve) IM<<- solve
  getmatrix<-function() IM
  list(set=set, get=get,
  setmatrix=setmatrix,
  getmatrix=getmatrix)
}

##2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
##should retrieve the inverse from the cache.

cacheSolve <- function(M=matrix(), ...) {
  IM<-M$getmatrix()
  ## the following "if loop" checks if the u-matrix-inverse had been
  ##already calculated (i.e. IM is not NULL), if yes it recalls it (IM) from memory to avoid the time consuming calculation
  if(!is.null(IM)){
    message("gettings cached data")
    return(IM)
  }
  matrix<-M$get()
  
  ##solve() is a R function that returns the matrix inverse
  IM<-solve(matrix, ...)
  M$setmatrix(IM)
  IM
}

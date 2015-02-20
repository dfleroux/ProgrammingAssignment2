
## >Purpose is to cache the reuslts of the 2  2 functions decribed below 
##in order to avoid repeating the same variable calculation thus saving computig time

##1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## <<- operator which can be used to assign a value to an object in an environment that 
##is different from the current environment

makeCacheMatrix <- function(u = matrix()) {
  s<-NULL
  set<-function(v){
    u<<-v
    s<<-NULL
  }
  get<-function() u
  setmatrix<-function(solve) s<<- solve
  getmatrix<-function() s
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

##2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
  s<-u$getmatrix()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  matrix<-u$get()
  s<-solve(matrix, ...)
  u$setmatrix(s)
  s
}

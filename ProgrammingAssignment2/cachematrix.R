## Computing inverse of matrices, doing a cache lookup if it has been already computed


## vector list of functions to set/get item from cache

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setInv<-function(newInv) inv<<-newInv
  getInv<-function() inv
  list(set=set, get=get, setInv=setInv,getInv=getInv)
}


## function to compute inverse of a matrix

cacheSolve <- function(x, ...) {
  
  inv<-x$getInv()
  if (!is.null(inv)) {
    ## returning cached data
    return(inv)
  }
  ## Return a matrix that is the inverse of 'x'  
  data<-x$get()
  newInv<-solve(data,...)
  x$setInv(newInv)
  newInv
  
}

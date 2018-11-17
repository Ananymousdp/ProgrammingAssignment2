## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<-y
  }
  get<-function() x
  setInv<-function(inverse) inv<<-inverse
  getInv<-function() inv
  list(get=get,set=set,setInv=setInv,getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv<-x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
  }
  data<-x$get()
  inv<-solve(data)
  x$setInv(inv)
  inv   ## Return a matrix that is the inverse of 'x'
}

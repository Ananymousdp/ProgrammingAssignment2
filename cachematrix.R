## Put comments here that give an overall description of what your
## functions do

## It helps us to assign,get matrix and sets and gets inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL ##Initializing inv to NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }##It sets y to x and inv to NULL in parent environment
  get<-function() x ##Gives input matrix
  setInv<-function(inverse) inv<<-inverse ##sets inverse matrix that has given as input
  getInv<-function() inv ##returns inverse of matrix value
  list(get=get,set=set,setInv=setInv,getInv=getInv) ##It allows to call methods as elements
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv<-x$getInv() ## Calls getInv() method defined in makeCacheMatrix function and assigns value to inv
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }##returns message if it finds value for inv and returns it
  data<-x$get() ##calls get method and returns input matrix
  inv<-solve(data,...)##calculates inverse of matrix using solve() method
  x$setInv(inv) ##calls setinv method and sets inverse
  inv ## returns inv of matrix
}

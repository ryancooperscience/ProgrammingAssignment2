## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix that is a list of values including the matrix
## and the setSolve, getSolve, and set function to set matrix

makeCacheMatrix <- function(x = matrix()) {
  sol<-NULL
  set <- function(y){
    x<<-y
    sol<<-NULL
  }
  get <- function(){
    x
  }
  setSolve <- function(solve){
    sol <<- solve
  }
  getSolve <- function() sol
  list(set=set,get=get,setSolve=setSolve, getSolve=getSolve)
}


## cacheSolve caches the solution of a matrix into the getSolve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  sol<-x$getSolve()
  if(!is.null(sol)){
    message('getting cached solution')
    return(sol)
  }
  data <- x$get()
  sol <- solve(data, ...)
  x$setSolve(sol)
  sol 
}

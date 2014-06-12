## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix acts like an object that holds the actual
## matrix along with its inverse if it has ever been calculated
## before, otherwise this inverse is set to NULL. If the matrix is
## changed via the `set` funcion, then the inverse is void to NULL.

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m   <<- y
    inv <<- NULL
  }
  get <- function() m
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## cacheSolve checks if the makeCacheMatrix passed as the first
## argument has the inverse matrix already calculated. If so, then
## this value is used and a message indicating cache hit is issue. 
## Otherwise the inverse is calculated, the result is stored in 
## makeCacheMatrix for further use, and the inverse is returned.

cacheSolve <- function(x, ...) {
  inver <- x$getInv()
  if (!is.null(inver)) {
    message ("Inverse matrix cache hit")
    return(inver)
  }
  inver<-solve(x$get())
  x$setInv(inver)
  inver
}

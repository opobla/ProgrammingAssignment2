## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

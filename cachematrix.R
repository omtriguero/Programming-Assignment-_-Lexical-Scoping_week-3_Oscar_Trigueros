## The next code calculate and save the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  In <- NULL# initilization values
  set <- function(y) {
    x <<- y  # Reset the exist cache  values
    In <<- NULL # Reset the exist cache  values
  }
  get <- function() x
  setInv <- function(invers) In <<- invers # invers is a dummy variable
  getInv <- function() In
  # like the makeVector, the return of makeCacheMatrix is a List of setter and getter 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

cacheSolve <- function(x, ...) {
  In <- x$getInv()
  if(!is.null(In)) {
    message("getting cached data") # if the cache find a inverse matrix, return the cache.
    return(In)
  }
  data <- x$get() # if the cache do not find a inverse matrix, calculate the new inverse matrix
  In <- solve(data, ...)
  x$setInv(In)
  In
}

h <- matrix(c(3,4,5,6,2,6,7,8,-1),3,3)

MyInveMat <- makeCacheMatrix(h)

cacheSolve(MyInveMat)
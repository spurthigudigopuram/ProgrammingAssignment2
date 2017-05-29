#The first function, makeCacheMatrix is a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

# cacheSolve checks if the inverse of a matrix has been calculated with the above function.
# If inverse has been not calculated it calculates using getinverse
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
    }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
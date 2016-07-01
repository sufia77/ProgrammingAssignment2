## There are two functions here. The first function is for create a special
## matrix that can cache its inverse and the second function is to compute
## the inverse of matrix created by first function.

## This function will call the matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will compute the inverse of matrix called by the above function.
## If the matrix is already cache then will show the message "getting cached data" along with cached inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

a <- diag(5, 6)
cacheMatrix <- makeCacheMatrix(a) #call the matrix
cacheSolve(cacheMatrix) # get the result as inverse of matrix



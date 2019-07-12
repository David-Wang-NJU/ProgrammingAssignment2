## Put comments here that give an overall description of what your
## functions do

##The following two functions will cache the inverse of a matrix


## Write a short comment describing this function

##This function returns a list of functions that can set and get the value of the matrix
##and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

##This function first checks whether the inverse of the matrix created by the above function
##already exists. If so, it gets the inverse and skips calculation. If not, it calculates
##the inverse and then returns it.
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


##The following code is a simple test
matrix1<-matrix(1:4,2,2)
matrix2<-makeCacheMatrix(matrix1)
cacheSolve(matrix2)



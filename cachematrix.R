## Functions to create a Matrix and then to calculate and cache the inverse of the matrix

## Function to create a matrix and get the value of the matrix. Incase a inverse for this matrix is already calulated 
## and available in cache, the function provides a way to get the value from the cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function to calulate the inverse of a matrix and cache the value for further use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Unit Testing - The below section has commands executed and the values returned during Unit testing

## z<-makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,4,0),nrow =3,ncol=3))
## z$get()
##[,1] [,2] [,3]
##[1,]    1    2    3
##[2,]    0    1    4
##[3,]    5    6    0

## z$getinverse()
## NULL

## cacheSolve(z)
##[,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1

## z$getinverse()
##[,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1

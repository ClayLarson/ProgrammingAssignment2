## The two functions, makeCacheMatrix() and “cacheSolve” are used invert ## a matrix.  Matrix inversion is slow for large matrices and the 
## functions here cache the inverse of a given matrix once it has been
## determined, this avoiding the need to recaluate.  Both functions
## assume that the matrix is invertable.    

## The makeCacheMatrix()function removes the cached inverse, and creates
## a matrix for caching which includes the original matrix x. The
## function returns a list of functions:  “set” – sets the value of the
##  matrix; “get” - get the value of the matrix; “setinverse” - set the
## value of inverse of the matrix; getinverse” - get the value of inverse
## of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL         ## initialize inv to NULL w/ each call to function
  
  set <- function(y) {## Note: the set function is never called  
    x <<- y
    inv <<- NULL
  }
  get <- function() x ## get function for underlying vector in R 
  setinverse <- function(inverse) inv <<- inverse ## Called by cachSolve  
  getinverse <- function() inv ## returns the inverse or NULL  
  list(set = set, get = get, ## function returns a list of functions 
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## The m cacheSolve()function is passed the list created by
## makeCacheMatrix. The function checks to see if inverse has been
## computed and if so, returns the cached value w/o recalculating it.
## otherwise it calculates the inverse of the original matrix using the
## solve() function.  

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv))            ## If inv is not NULL, use cached value 
  {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get() ## call get(),which returns the underlying vector x 
  inv <- solve(data, ...)
  x$setinverse(inv) ## cache the inverse using setinverse() 
  inv               ## display the inverse 
}

##EXAMPLE makeCacheMatrix() & “cacheSolve” functions:
## > x<-matrix(c(1:4),2,2)
## > xx<-makeCacheMatrix(x) 
## > xx$get() 
##        [,1] [,2] 
##   [1,]    1    3 
##   [2,]    2    4 
## > xx$getinverse() 
##   NULL  
## > cacheSolve(xx) 
##          [,1] [,2] 
##     [1,]   -2  1.5 
##     [2,]    1 -0.5 
## > cacheSolve(xx)
## getting cached data
##          [,1] [,2] 
##     [1,]   -2  1.5 
##     [2,]    1 -0.5 
## > xx$getinverse() 
##        [,1] [,2] 
##   [1,]   -2  1.5 
##   [2,]    1 -0.5 
## > solve(x) 
##        [,1] [,2] 
##   [1,]   -2  1.5 
##   [2,]    1 -0.5 



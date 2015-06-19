## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


##The first function ,MakeCacheMatrix creates a special "matrix" that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y=matrix()){
    x <<- y
    y <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<-i
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The function , cachesolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    print("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}

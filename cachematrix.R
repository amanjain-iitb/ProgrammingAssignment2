## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   ## Checking Invertability     
   if (ncol(x)==nrow(x) && det(x)!=0) {
    t <- NULL ##var for inverse
    set <- function(y) {
      x <<- y
      t <<- NULL
    }
    get <- function(){
      x
    } 
    setinverse <- function(inverse){
      t <<- inverse
    } 
    getinverse <- function(){
      t
    } 
    list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
  }  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  t <- x$getinverse() ##gets inverse
  if (!is.null(t)) { ## checks if inverse already exists and if yes it retrives it
    message("getting cached data")
    return(t)
  }
  data <- x$get()
  t <- solve(data, ...) 
  x$setinverse(t) ##sets inverse
  t
}

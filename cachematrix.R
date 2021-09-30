## Together these two functions cache and return the inverse of an invertible matrix

## This function makes a special matrix that is really a list
## containing functions to
## first set the matrix, by setting x in the parent environment to the input argument of the set function
## get the matrix, to return the matrix
## set the matrix inverse, by solving for the inverse i of the matrix x
## get the matrix inverse, to return the inverse i

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


##cacheSolve takes a makeVector() function as its argument
##and executes the retrieval of a cached inverse or solving the inverse of the matrix

cacheSolve <- function(x, ...) {
  
  #first the function calls the getinverse() function on the input argument x
  i <- x$getinverse()
  
  #next the function checks to see if the inverse has already been cached
  #if the inverse is in the cache, i will not be null, so i will be returned 
  #with a message to indicate the cached data was retrieved
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #if i is null, the inverse is solved
  #the function retrieves the matrix x using the get() function
  #then assigns i to be the result of the solve function
  #finally, it sets the inverse of x to be i to have the result cached
  #and returns the inverse i
  else
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

#test code
myMatrix <- makeCacheMatrix(x=matrix(c(-1,1,3/2,-1), nrow = 2, ncol = 2))
myMatrix$get() #returns matrix 
myMatrix$getinverse() #null
cacheSolve(myMatrix)
myMatrix$getinverse() #now that cacheSolve() has been called, can get inverse
cacheSolve(myMatrix) #confirmed that cacheSolve() retrieves data from the cache, sends message "getting cached data"

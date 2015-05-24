## Christina , May 2015
## R Programming Coursera Course, Programming Assignment2
## The two functions below calculate the inverse of an input matrix, returning the cached value if available

## makeCacheMatrix contains four helper functions
#set changes the matrix in the main function
#get is the matrix from the main function
#setinverse sets the inverse in the main function
#getinverse returns the inverse from the main function

makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  
  #set
  set <- function(y) {
    x <<- y #set the matrix in the main function this one 
    inversematrix <<- NULL #reset inverse to NULL. It needs to be calculated on the new matrix
  }
  
  #get 
  get <- function() x
  
  #setinverse
  #note this function doesn't actually calculate the inverse. It merely sets it to whatever is passed in.
  setinverse <- function(inv) inversematrix <<- inv
  
  #getinserse
  getinverse <- function() inversematrix
  
  #this makes the function return a list of functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of the input matrix, where the input matrix was created using function 'makeCacheMatrix'
# if the matrix inverse has previously been calculated and is available in memory, return it.
# otherwise, calculate the inverse now.

cacheSolve <- function(x, ...) {
  
  inversematrix <- x$getinverse() # current inverse for this matrix
  #if it is not null, then we have already calculated it and cached it
  if(!is.null(inversematrix)) {
    message("getting cached data")
    #if we already have the inverse, then have the function return it and exit
    return(inversematrix)
  }
  
  #else, the inverse is null, we have not yet calculated it.
  data <- x$get() #first, get the matrix
  inversematrix<- solve(data, ...) #then calculate the inverse
  x$setinverse(inversematrix) #then set the inverse in the function, so we have it cached for future use.
  inversematrix #now return the inverse
}


#

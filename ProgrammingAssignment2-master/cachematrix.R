## ProgrammingAssignment2

##Finding the inverse of a matrix can be a time-consuming computation.
#Caching the inverse of a matrix allows you to find the 'saved' values 
#rather than recomputing. The following functions find and cache the 
#inverse of a function. 
#(If square matrix, remember can just use solve())


##makeCacheMatrix -Makes a special 'matrix' that can cache the inverse
#Use the setup from the example; follow the same format 
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  #Makes a special vector that can cache the inverse
  
  # Initially set invr equal to NULL
  #invr will be changed to the inverse matrix
  invr <- NULL
  
  #set function will assign new values to matrix 
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  
  # get function retrieves matrix
  get <- function() x
  
  # Set the inverse by assinging invr values
  setinverse <- function(inverse) invr <<- inverse
  
  # Get the inverse
  getinverse <- function() invr
  
  # Create a list that can be called
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)	
}




##Returns inverse of special matrix from makeCacheMatrix
#If inverse has been calculated, retrieves inverse from cache
#or calculates inverse of matrix

cacheSolve <- function(x, ...) {
  ##Return matrix that is the inverse of 'x'
  #Use the setup from the example; follow the same format
  
  # Get the current inverse
  invr <- x$getinverse()
  
  #If there is an inverse computed, return computed inverse
  if(!is.null(invr)) {
    message("Getting cached data")
    return(invr)
  }
  
  # If no matrix computed, get matrix
  data <- x$get()
  
  # Find the inverse using solve()
  invr <- solve(data, ...)
  
  # Use setinverse to cache this inverse data
  x$setinverse(invr)
  
  # Return inverse matrix
  invr    
}


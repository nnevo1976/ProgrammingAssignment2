#Assignment 2: Caching the Inverse of a Matrix

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# Creation of matrix object

makeCacheMatrix <- function( m = matrix() ) {
  
  # To initialize
  i <- NULL
  
  # Set the value of the matrix 
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  # Set the value of the inverse
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  # Get the value of the inverse
  getInverse <- function() {
    ## Returns the inverse
    i
  }
  
  # Creating a list containing a function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse
#from the cache

cacheSolve <- function(x, ...) {
  
  # Calculate a matrix m- the inverse of x 
  m <- x$getInverse()
  
  # Return the set inverse
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  # Get the matrix from the cashe
  data <- x$get()
  
  # Calculate the inverse 
  m <- solve(data) %*% data
  
  # Set the inverse 
  x$setInverse(m)
  
  # Returns the matrix
  m
}
# Testing
T <- matrix(c(1,3,3,4,1,6,7,8,2), nrow=3, ncol=3)
solve(T) 
T1 <- makeCacheMatrix(T)
cacheSolve(T1)



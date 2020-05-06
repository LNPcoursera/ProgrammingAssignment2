## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function

# Function makeCacheMatrix: This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function( x = matrix( ) ) {
  
  i <- NULL
  set <- function( y ) 
  { # Set the matrix
    x <<- y
    i <<- NULL
  }
  
  get <- function( ) x # Get the matrix
  
  setinverse <- function( inverse ) i <<- inverse  # Set the inverse matrix
  
  getinverse <- function( ) i # Get the inverse matrix
  
  # Methods
  list( set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse )
}

## Write a short comment describing this function

# Function cacheSolve: This function computes the inverse of the matrix returned by the function makeCacheMatrix described previously. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve function should retrieve the inverse from the cache.

cacheSolve <- function( x, ... ) {
  
  
  i <- x$getinverse( ) # Return an inverse matrix of 'x'
  
  # Return the inverse matrix  
  if ( !is.null( i ) ) 
  {
    message( "Getting cached data" )
    return( i )
  }
  
  data <- x$get( ) # Get the matrix
  
  i <- solve( data ) # Solving the matrix multiplication 
  
  x$setinverse( i ) # Adding to the object
  
  return( i ) # Return the inverse
  
}
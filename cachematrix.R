makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize a variable to store the inverse of the matrix
  
  set <- function(y) {
    x <<- y  # Set the matrix
    inv <<- NULL  # Since the matrix is altered, reset the inverse to NULL
  }
  
  get <- function() x  # Retrieve the matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Set the inverse of the matrix
  
  getInverse <- function() inv  # Retrieve the inverse of the matrix
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  # Return a list of the functions
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached inverse")  # Inform that we are retrieving the cached inverse
    return(inv)
  }
  
  data <- x$get()  # Get the matrix from our special “matrix” object
  
  inv <- solve(data, ...)  # Calculate the inverse
  x$setInverse(inv)  # Store the inverse in our special “matrix” object
  
  inv  # Return the inverse
}

# Create a sample matrix
mat <- matrix(c(1, 2, 3, 4), nrow = 2)

# Create a cacheMatrix object
cacheMat <- makeCacheMatrix(mat)

# Calculate and cache the inverse
inverse <- cacheSolve(cacheMat)
print(inverse)

# This should retrieve the inverse from cache
inverse_again <- cacheSolve(cacheMat)


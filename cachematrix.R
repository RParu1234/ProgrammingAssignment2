## Comments that give an overall explanation of the program

## Programme Assigment 2 - CourseEra - Functions
## There are two functions expalined here
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


makeCacheMatrix <- function(x= matrix()){
  # initializing Inverse as NULL
  
  inv <- NULL
  
  # Function to set Inverse as NULL
  
  set <- function(y){
    
    x<<-y
    inv <<- NULL
  }
  
  # Function to get Inverse of the Matrix
  
  get <-function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

## Function to get the cached data

cacheSolve <- function(x, ...) { 
  
  ## Return a matrix that is the inverse of 'x'

    inv <- x$getInverse()
  
  # Checking if the Inverse is NULL
    
    if (!is.null(inv)) {
    message("getting cached data")
    return(inv)                       # Returns the Inverses value
    }

## calculating the Inverse Value of the matrix  
      
  compmat <- x$get()
  inv <- solve(compmat, ...)
  x$setInverse(inv)
  inv
}

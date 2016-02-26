## Description
##
## makeCacheMatrix and cacheSolve allows to optimize access to inverse of a given matrix
##
## Usage
##
## having matrix x call makeCacheMatrix(x) to convert to cacheable object
## pass this object to cacheSolve to retrieve inverse matrix value (calculated only once)
##
## Code sample
##
## set.seed(22) # initialize seed for repeatable random number generation
## randomSquareMatrix <- matrix(rnorm(16),4,4) # create 4x4 matrix with random values
## cached <- makeCacheMatrix(randomSquareMatrix) # initialize cache
## cacheSolve(cached) # for this call (as there are no cached values) inverse will be calculated and cached
## cacheSolve(cached) # for this call inverse will be retrieved from cache
## Note: matrix that is passed needs to be inversible (square)

## Description
## Converts given matrix to cacheable object capable of store cache of inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  
  # variable that will be used as a cache for transposed matrix
  inversed <- NULL
  
  # sets new matrix to be inversed AND clean cached value
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  
  # return original matrix
  get <- function() x
  
  # set inversed matrix value (cached)
  setInversed <- function(value) inversed <<- value
  
  # return cached value
  getInversed <- function() inversed
  
  # return list of functions that can be used to operate on original and cached data
  list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
  
}


## Use object created with makeCacheMatrix to retrieve inverse of matrix
## inverse is calculated in 'lazy way' - i.e. first call calculates inverse consecutive calls will retrieve cached data
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversed <- x$getInversed()
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  data <- x$get()
  inversed <- solve(data)
  x$setInversed(inversed)
  inversed
}



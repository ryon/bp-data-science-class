# https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping

# makeCacheMatrix:
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # initialize internally-stored inverse
  .inverse <- NULL

  # create setter for x
  set <- function(y) {
    x <<- y
    .inverse <<- NULL
  }

  # create getter for x
  get <- function() {
    return(x)
  }

  # create setter for .inverse
  setInverse <- function(inverse) {
    .inverse <<- inverse
  }

  # create getter for .inverse
  getInverse <- function() {
    return(.inverse)
  }

  # return object with our getters and setters
  return(
    list(
      set = set,
      get = get,
      setInverse = setInverse,
      getInverse = getInverse
    )
  )
}

# cacheSolve:
# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and the
# matrix has not changed), then the cachesolve should retrieve the inverse from
# the cache.

cacheSolve <- function(x, ...) {

  # if there's an inverse stored already, fetch it
  # ----------------------------------------------

  # get inverse from x if available
  .inverse <- x$getInverse()

  # if it exists, just return it with a message
  if (!is.null(.inverse)) {
    message('Getting cached data...')

    return(.inverse)
  }

  # otherwise, calculate it and cache it
  # ----------------------------------------------

  # grab x's matrix and calculate its inverse
  .y <- x$get()
  .inverse <- solve(.y, ...)

  # store the inverse back in x
  x$setInverse(.inverse)

  return(.inverse)
}

# BACKGROUND
# ------------------------------------------------------------------------------

# The first function, makeVector creates a special "vector", which is really a
# list containing a function to
#
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# The following function calculates the mean of the special "vector" created
# with the above function. However, it first checks to see if the mean has
# already been calculated. If so, it gets the mean from the cache and skips the
# computation. Otherwise, it calculates the mean of the data and sets the value
# of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

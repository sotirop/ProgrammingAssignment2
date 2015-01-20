# ---Overall description---
# The function makeCacheMatrix creates an environment that keeps the cache and 
# provides the tools to access/alter the cache.
# The function cacheSolve uses the tools of the makeCacheMatrix function in
# order to check whether a cached value exists. If yes, it returns the cached
# value. If there is no cached value (NULL), then it calculates the inverse
# matrix and stores it in the cache.

# --- function makeCacheMatrix ---
# The function makeCacheMatrix takes one matrix as input and returns a vector as
# output. This function has a "special" variable called cache whose value can be
# accessed by the functions that are defined in the same environment. These 
# functions are: set, get, set_cache, get_cache. The output vector is a vector
# with these four functions. So when you use the functions from this vector, you
# can access and alter the value of the cache.

makeCacheMatrix <- function(x = matrix()) {
    # cache is the cache
    cache <- NULL
    
    # function set sets the value of the matrix
    set <- function(y) {
      x <<- y
      cache <<- NULL
    }
    
    # function get returns the value of the matrix
    get <- function(){
      x
    }
    
    # function set_cache sets tha value of the cache
    set_cache <- function(m){
      cache <<- m
    }
    
    # function get_cache returns the value of the cache
    get_cache <- function(){
      cache
    }
    
    # this command return a vector with the four functions that 
    # were defined above.
    list(set = set, get = get,
         set_cache = set_cache,
         get_cache = get_cache)
}

# --- function cacheSolve ---
# The function cacheSolve uses the functions from the makeCacheMatrix returned vector
# in order to check whether a cached value exists. If yes, it returns the cached
# value. If there is no cached value (NULL), then it calculates the inverse
# matrix and stores it in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # First we execute function get_cache, which returns the contents of the
  # cache. If the function is not NULL, then the cache was not empty, so the
  # cache already had the result we wanted, so we print a nice message and
  # return the result from the cache.
    im <- x$get_cache()
    if(!is.null(im)) {
      message("getting cached data")
      return(im)
    }
    
  # We reached this point, so the cache was empty. We have to get the actual 
  # data (that is the original matrix) in order to calculate the inverse matrix.
  # The variable data gets the matrix. Then we use the command 'solve' to 
  # calculate the inverse matrix and store the result in im. We also set the 
  # cache with the calculated value. Last, we return this value.
    data <- x$get()
    im <- solve(data, ...) # <---- This is supposed to take a long time to execute
    x$set_cache(im)
    im
}

# Example usage:
# We define a matrix mat
# > mat <- matrix(c(4,3,3,2), 2, 2)
# > mat
#      [,1] [,2]
# [1,]    4    3
# [2,]    3    2
# 
# Then we get the vector with the four functions and an environment which has the
# cache:
# > vec <- makeCacheMatrix(mat)
# 
# We execute the command and we get the inverse matrix
# > cacheSolve(vec)
#      [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
#
# If we execute the same command again, we get a cached version of the result
# > cacheSolve(vec)
# getting cached data
#      [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
#
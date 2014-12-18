# Darrell Thomas
# R Programming
# Coursera.org, December 2014

# notes on execution:
# not required, but the following console commands may be helpful to execute this code:
#
# source("cachematrix.R")
# mat1 <- makeCacheMatrix(matrix(rnorm(1:9), 3,3))
# cacheSolve(mat1)
# cahceSolve(mat1)
#

#=======================================================================
# makeCacheMatrix() takes x as input, (a numeric matrix). By 
# creating a matrix with makeCacheMatrix(), not only
# does it "make" the matrix, it attaches these additional
# functions (objects) to be used later. 
# To use a Christmas analogy, think of the vector itself as the 
# Christmas tree, and all of the little functions
# --and the list-- at the bottom as ornaments on the tree.  
# The calling function, called cacheSolve().


makeCacheMatrix <- function(x = matrix()) {
  
  
# my_inverse is the 'inverse' and it is initialized to NULL at every call
# needs to be named same in other function, because it is accessed from the
# parent scope by both functions.
  
  my_inverse <- NULL         

  
# note these next three functions are defined, but not called 
# anywhere here... they are used later in cacheSolve() to get 
# values for x, or for my_inverse, and for setting and storing the
# inverse. To access these functions from elsewhere, one way is to 
# use the following  format to access the list created below:
#
#  z <- x$getinverse()
#  z <- x$setinverse()
#
#  etc...
#

# This function just returns the value of the original matrix

  getmatrix <- function() {x}
  

# This function is called by cacheSolve() during the first makeCacheMatrix()
# access, and it will store the value using superassignment
# the 'superassignment' just stores mean in m, one parent scope up,
# so that other code (outside of this function) has access to it.

  setinverse <- function(solve) { my_inverse <<- solve }
 
# This will return the cached value to cacheSolve()
# on subsequent access.  If it's never been set,
# it would return NULL.  If it has been previously
# set, it would return the cached value.

  getinverse <- function() {my_inverse}
  
  
# This list is accessed each time makeCacheMatrix() is called.
# This is a 'list' of the internal functions.  This makes it 
# so a calling function knows how to access these functions.

  list( getmatrix  = getmatrix,
        setinverse = setinverse,
        getinverse = getinverse  )
  
}


##=======================================================================

# This is the calling function to use a matrix
# created by makeCacheMatrix(), and it's associated functions.
# The input to this function (x) is an object 
# created by makeCacheMatrix()

cacheSolve <- function(x, ...) {
  
  # Access this function (object) located in makeCacheMatrix()
  # recall that getinverse() is just an object in the list
  # created inside of makeCacheMatrix().
  # if an inverse had never been calculated, this would
  # return NULL, if it had been calculated, it would 
  # return the cached value.
  
  my_inverse <- x$getinverse()
  
  # if mean was already cached (not NULL)
  
  if(!is.null(my_inverse)) {
    
    # then write the message to notify 
    
    message("getting cached data")
    
    # and return the cached value of m, as previously 
    # collected by getmean()
    
    return(my_inverse)
  }
  
  # This section of code would only be run if the mean 
  # were NULL. So, first, let's go get the data from 
  # the function created with makeVector()
  
  data <- x$getmatrix()
  
  # Now, use that vector to calculate the mean.  This 
  # is where the actual calculation takes place. **finally**!
  
  my_inverse <- solve(data, ...)
  
  # Now that we have a calculated (non-NULL) value for 
  # let's push it to the cache, by using the setmean()
  # function. 
  
  x$setinverse(my_inverse)
  
  # return the value calculated.
  
  my_inverse
  
}
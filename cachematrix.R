## Put comments here that give an overall description of what your
## functions do

# This is a set of two functions that allow to cache the results of a matrix
# inversion calculation.
# Use:
# In an initial step generate a cache for the matrix and its inverse 
# with "matrixCache <- makeCacheMatrix(originalMatrix)".
# Whenever the inverse is needed, request the inverse from "cacheSolve" 
# using the cache object: "inv <- cacheSolve(matrixCache)" 
# cacheSolve will check the cache and return the cached inverse if it exists.
# If not it will calculate it, cache, and return it.
# Note:
# In case the original matrix changes, you will need to update 
# it in the cache object, otherwise the wrong inverse will be returned


## Write a short comment describing this function
# makeCacheMatrix(m)
# returns a list of functions to access a cached matrix (m) and its inverse
# Use:
# Generate a new matrix-cache object with 
# matrixCache <- makeCacheMatrix(originalMatrix)
# the object matrixCache allows to 
# 1. access the original matrix with a getter function: orig <- matrixCache$get()
#    the original matrix is set in the initial call to makeCacheMatrix(orig)
# 2. update the original matrix with a setter function: matrixCache$set(newMatrix)
# 3. access the inverse of the matrix with: inv <- matrixCache$getinv()
#    the getter will return NULL if inverse has not been set yet (see 4.)
# 4. and set the inverse once calculated with: matrixCache$setinv(inv)
#

makeCacheMatrix <- function(m = matrix()){
     # m = to cache (with inverse)
     # returns list of four functions needed to cache data in an object
     # Use: 
     # assign list to a new object to later call the functions 
     # to retrieve or set the cached data
     
     inv <- NULL              # initialize inverse to NULL
     set <- function(mat)     {
          m <<- mat			# save matrix mat in m in private env
          inv <<- NULL		# reset inv (inv = inverse matrix)
     }
     get <- function() {		# shorter: get <- function() x
          m		     	# return original matrix
     }
     setinv <- function(mat)	{
          inv <<- mat		# set matrix in its own env
     }
     getinv <- function()	{
          inv		     	# return inv matrix from cache
     }
     # return list of getter and setter functions
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# cacheSolve(m)
# returns the inverse of a matrix stored in matrix-cache object m
# uses the cached inverse if it has been saved already
# otherwise calculates the inverse, stores it, and returns it
# Use:
# mCache <- makeCacheMatrix(originalMatrix)
# inv <- cacheSolve(mCache)
# first call: will calcluate the inverse
# inv2 <- cacheSolve(mCache)
# second call: will access cache 
# Note:
# Need to update matrix cache if matrix has changed
# mCache$set(newMatrix)
# this will reset the cache

cacheSolve <- function(m, ...)     {
     # m is object with matrix data and a function list 
     # of type = makeCacheMatrix
     
     inv <- m$getinv()             # get the inverse from object
                                   # NULL if not yet cached
     if (! is.null(inv)) {         # if the inverse was already cached
          message("returning cached inverse")
          return(inv)
     }
     mat <- m$get()                # get matrix saved using function in object m
     inv <- solve(mat)             # calculate its inverse 
                                   # - assuming matrix supplied is invertible
     m$setinv(inv)                 # save inverse
     inv                           # return inverse
}

## Programming Assignment 2

## Write the following functions:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## For this assignment, assume that the matrix supplied is always invertible.

#------------------------------------------------------------------------------------------

## Put comments here that give an overall description of what your
## functions do

# Function 1

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL #initialized cache inverse to NULL
  
  set <- function(y) { #initializing function to create a new matrix
    x <<- y # set new
    inverse <<- NULL # reset if matrix changes
  }
  
  get <- function() x # to get the current  matricx
  
  set_inverse <- function(inv) inverse <<- inv # cache the inverse
  
  get_inverse <- function() inverse # retrieve the cached inverse
  
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse) # return all four functions as a list
  
}

# Function 2 

cacheSolve <- function(x, ...) {
  
  # first check if the inverse has already been cached
  inv <- x$get_inverse()  
  
  # return it if it has been cached
  if(!is.null(inv)) { 
    message("getting cached data")
    return(inv)
  }
  
  # if inverse hasn't been cached, calculate it:
  matrix <- x$get() # get matrix
  
  inv <- solve(matrix, ...) # compute the inverse
  
  x$set_inverse(inv) # cache it
  
  inv # return it
}

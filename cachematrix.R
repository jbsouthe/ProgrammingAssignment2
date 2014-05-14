## This file contains the functions needed to implement and work with a 
##special class of inverse cachable matrix

## This function creates a special class of matrix that can 
## cache the calculated inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL #this will hold the cached inverse of the matrix
  set <- function(m) { #mutator to set the matrix and reset the cache
    x <<- m
    inverse <<- NULL
  }
  
  get <- function() { return(x) } #accessor to retrieve the matrix
  
  #these methods mutate and access the cached inverse variable
  setInverse <- function(inv) { inverse <<- inv }
  getInverse <- function() { return(inverse) }
  
  #list of methods available in this function/object
  list( set = set, get = get, setInverse = setInverse, 
        getInverse = getInverse )
}


## This function calculates the inverse of a given matrix and 
## caches that value with the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse() #first retreive the cached var
  if(!is.null(inverse)) { #if it is already set to a value, return it
    message("getting cached data")
    return(inverse)
  }
  
  #otherwise, we continue with execution and calculate then set it
  matrix <- x$get() #retrieve the matrix from x
  if( det(matrix) == 0 ) { #test to insure an inverse exists, or not
    x$setInverse(NA) #if det == 0 then no inverse
  } else {
    x$setInverse( solve(matrix) ) #otherwise set matrix to inverse via solve()
  }
  return(x$getInverse()) #explicitly return the cached inverse, or NA
}

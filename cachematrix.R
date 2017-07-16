makeCacheMatrix <- function(a = matrix()) {
  ## @a: invertible matrix
  ## return: a list containing functions to
  ##1. set the matrix
  ##2. get the matrix
  ##3. set the inverse
  ##4. get the inverse
  ##this list is used as the input to cacheSolve()
  
  inv <- NULL
  set <- function(b) {
    # use <<- to set a equal to b in parent enviornment  
    a <<- b
    inv <<- NULL
  }
  get = function() a
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(a, ...) {
  ## @a: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv <- a$getinv()
  
  # calculate the value of inv if inv is undefined 
  if (!is.null(inv)){
    # if record is in cache, fetch data
    message("getting cached data")
    return(inv)
  }
  
  # if the inv value is undefined, then calculate inverse 
  get_matrix <- a$get()
  inv <- solve(get_matrix, ...)
  
  # set inverse in cache
  a$setinv(inv)
  
  return(inv)
}
}
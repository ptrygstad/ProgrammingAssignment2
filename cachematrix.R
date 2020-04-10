    ## makeCacheMatrix() creates a special matrix that can be used
    ## to get and set both the value of the matrix and the value 
    ## of the inverse of the matrix

    ## cacheSolve checks if there is a cached inverse of input argument
    ## if not, cacheSolve calculates inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y  
    i <<- NULL 
  }
      ## set() assigns the input argument to x in the parent environment and 
      ## clears any value of i previously cache when x is reset
  
  get <- function() x
  seti <- function(inverse) i <<- inverse
  geti <- function() i
  list(set = set, get = get,
       seti = seti,
       geti = geti)
  
      ## get() gets value of matrix x
      ## seti() sets value of inverse of matrix x
      ## geti() gets value of inverse of matrix x
      ## list assigns each function as an element of a list
      ## returns list
}


cacheSolve <- function(x, ...) {
  i <- x$geti()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
      ## checks if there is already a cached inverse of the object
      ## passed as an arguement
      ## if (!is.null(i)) = FALSE, return cached inverse
  
  data <- x$get()
  i <- solve(data, ...)
  x$seti(i)
  i
      ## return a matrix that is the inverse of 'x'
}

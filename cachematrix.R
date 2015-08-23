## Updated by Michael Farrell / 22-Aug-2015
## Set of two functions which calculate and store the inverse of a matrix.  Values are
## are cached so they can be leveraged without repeating potentially costly processing time.

## This function calculates and stored inverse of matrix.  Cached value is stored within 'inv'.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    #  This will be the matrix inverse.  It will be reset to NULL each time makeCacheMatrix is called.
  set <- function(y) {
    x <<- y
    inv <<-NULL
  }
  get <- function() { x }   # Returns the value of the original matrix
  setinverse <- function(solve)  { inv <<- solve }  # Called by cacheSolve if result set isn't cached.
  getinverse <- function() { inv } # Returns the cached value to cacheSolve() on subsequent accesses.
  list(get = get,          #  This is accessed each time makeCacheMatrix() is called,       
       setinverse = setinverse,  
       getinverse = getinverse)                        
}

## This function first checks to see if the matrix inverse has already been calculated.  If it has then it returns 'inv' value.
## If the 'inv' value is null, then matrix inverse is calculated.

cacheSolve <- function(x, ...) {   # The input x is an object created by makeCacheMatrix.  Defined by the <<- operator.
  inv <- x$getinverse()            # Accesses the object 'x' and gets the inverse of the matrix
  if(!is.null(inv)) {              # If matrix inverse was already cached (not NULL) then...
    message("getting cached data")  # This message is sent to the console.
    return(inv)                       # Returns the inverse ... "return" ends the function cacheSolve().
  }
  data <- x$get()        # If x$getinverse() returned NULL, not cached, then matrix inverse is calculated.
  inv <- solve(data, ...) 
  x$setinverse(inv)      # Stores the inverse values in x (see setinverse() in makeCacheMatrix function).
  inv                   
}
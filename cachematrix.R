##      ==> makeCacheMatrix function:
##>>creates a list of functions:  
##    >To get and set the original matrix (to be inversed)
##    >To get and set the cached inverse of a matrix

##      ==> cacheSolve
##>>check cache
##    >if cache !empty, get the cached inverse
##    >if cache empty (inverse hasn't been calculated and cached before), calculate the inverse and cache the inverse


makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix to be inversed
  inv <- NULL
  set <- function(y) {
    x<<-y
    inv<<-NULL ##the inverse hasn't been calculated yet and is therefor cached as NULL
    }
  
  ## get the value of the matrix to be inversed
  get <- function() x ## get just returns the original matrix to be inversed
  
  ## Set the value of the inversed matrix
  setinv<- function(solvedinv) inv<<-solvedinv
  
  ## Get the value of the inversed matrix
  getinv<- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x':
  
  ##call the makecachevector of function, more specifically getinv
  inv <- x$getinv()
  ##check if the matrix has been calculated before, i.e. if the inverse has already been cached
  if(!is.null(inv)) {
    message("getting cached data")
    ##break out of the function and return the cached inverse matrix
    return(inv)
    }
  ##if the result of the getinv() function was NULL, then calculate the inverse by using the solve() function and putting the result in cache
  data <- x$get()
  ##calculate the inverse matrix
  inv <- solve(data)
  ##put the calculated inverse matrix in cache
  x$setinv(inv)
  ##return the calculated inverse
  inv
}

## Unit tests:
 x <- rbind(c(1, -1/4), c(-1/4, 1)) 
 m<-makeCacheMatrix(x) 
 m$get()
 x<-cacheSolve(m)
 x

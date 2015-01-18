## Function makeCacheMatrix() takes matrix as a input argument 
#  (for example matrix stored in object d), computes its inverse and stores
#it as the object "inv". Function cacheSolve() accessing this object "inv" via getinverse() function
#from cache. This is very useful in complicated loop computations, where we need
#to use use matrix inverse many times. This way  function cacheSolve() accessing already
##computed inverse without calculating it at every loop.

## 

## function makeCacheMatrix() takes invertable matrix as the argument, computes its
#inverse and stores it in object "inv". Outputs list of 4 functions

##
makeCacheMatrix <- function(x = numeric()) {
      inv<- NULL #setting object that will hold matrix inverse to 0.
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve #computes inverse of input argument 
                                                  #matrix and stores to object inv
      getinverse <- function() inv
      list(set = set, get = get,   #returns list of 4 functions
           setinverse = setinverse,
           getinverse = getinverse)
}


#Function CacheSolve() takes list of returned makeCacheMatrix() functions as its arguments, then look for
#cached inv value in parent environment.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      #Function cacheSolve looks for computed inverse which is stored in object inv
      inv <- x$getinverse() 
      if(!is.null(inv)) { #if the inv values is not zero, returns its value using getinverse() function
            message("getting cached data")
            return(inv)
      }
      data <- x$get() #if the inv value is zero (computes inverse 1 time), then calculates matrix inverse
                      #and sets its value to object "inv" via setinverse() function.
      inv <- solve(data, ...) 
      x$setinverse(inv)
      inv
}


d<-matrix(rnorm(25),5,5)


 

 
## The MakeCache Matrix will create a new object of type MakeCacheMatrix.
## This object has accessor and mutator methods. The computed inverse of the input matrix will be stored to the Cache object via the set_inverse method.
## The get_inverse method will retrive the stored value


## Calls in the console should be in the follwing sequence.
## result <- makeCacheMatrix(matrix (x:y,nrows,ncols)) #square matrix only.
## result$get_inverse --> will be null the first time.
## cacheSolve(result)
## result$get_inverse() should return the value of the cached Inverse function.

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set<- function(y){
    x<<-y
    m<<-NULL 
  
  }
  get<- function() x
  
  set_inverse <- function(inverse) m<<- inverse
  get_inverse <- function() m
  
  # return the special constructor object.
  
  list(set = set, get= get, set_inverse = set_inverse, get_inverse = get_inverse)
  
}


## CacheSolve uses the library function Solve to caluclate the inverse of the square matrix.

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'x'
 
 
  m<- z$get_inverse()
  
  if(! is.null(m)){
    message("Getting the Cached Inverse of the Square matrix")
    return (m)
  }
  my_data <- z$get()
  #compute inverse and store it in the m which is found in the parent environment.
  m<- solve(my_data)
    # set the inverse matrix into a cache.
  
  z$set_inverse(m)
  # return value
  m
}

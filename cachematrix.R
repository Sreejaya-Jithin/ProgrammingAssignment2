#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function(){
    x
    }
    
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function(){
          inverseMatrix
          }
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.



  cacheSolve <- function(x) {
 
  inverseMatrix <- x$getInverse()
  if (!is.null(inverseMatrix)) {
    message("getting inverse from cached matrix")
    return(inverseMatrix)
  }
  mat <- x$get()
  inverseMatrix <- solve(mat)
  x$setInverse(inverseMatrix)
  return (inverseMatrix)
}


makeCacheMatrix <- function(x = matrix()){
          m <- NULL
          set <- function(y) { ##Set Value of Function
            x <<- y
            m <<- NULL
          }
          get <- function()x  ##Getting Function 
          setinverse <- function(inverse) m <<- inverse ## setting value of the inverse of the matrix
          getinverse <- function() m ##getting the value of inverse of the matrix 
          list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

cacheSolve <- function(x, ...) {   ##running function
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data.") ##checking to see if answer is cached
    return(m)
  }
  data <- x$get()   ##if not runs computation 
  m <- solve(data)
  x$setinverse(m)
  m
}
x <- matrix(c(1,2,1,3), nrow=2, ncol=2)
x
p = makeCacheMatrix(x)
p$get()
cacheSolve(p)
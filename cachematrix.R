## This function cache the inverse of square matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
    x<<-y
    m<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) m<<- solve
    getinverse<-function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function return the inverse of matrix. 
## If the inverse has been calculated previouslly, the program sent a message a get the matrix from cache. 
## Else, the program calculates the inverse.


cacheSolve <- function(x, ...) {
    m<-x$getinverse()
    if(!is.null(m)){
      message("getting cached matrix")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setinverse(m)
    m
}

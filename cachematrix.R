## makCacheMatrix is a function that accepts a matrix as input and 
## returns a list containing 4 functions:
##    set
##    get
##    setinverse
##    getinverse
##
##
## Each function can be called to perform operations on the matrix 
## that is stored in memory by this function
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
  
        get <- function () x
  
        setinverse <- function(inv) m <<- inv
  
        getinverse <- function() m
  
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}




## cacheSolve is a function that accepts an invertible matrix as input
## and returns the inverse of the matrix as input
## The matrix it receives as input needs to have been been created by the
## makeCacheMatrix function
## 
## cacheSolve will first look in memory to see if the inverse has alreadby
## been cached and if so, it will return it
##
## if the inverse has not been cached, it will call a function to 
## calculate the inverse and then return it
## 

cacheSolve <- function(x, ...) {
  
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached inverse")
        return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


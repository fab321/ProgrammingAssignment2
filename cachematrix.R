## makeCacheMatrix can store a matrix as well as 
## is inverse. By using the cacheSolve function, 
## the inverse will only be calculated once, and each 
## subsequent call will simply output the previously calculate 
## the stored result of the inverse


## makes a list of functions used to set and get a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    set <- function(y){
       x <<- y
       inv <<- NULL
    }
    
    getMatrix <- function(){
        x
    }
    getInverse <- function(){
        inv
    }
    setInverse <- function(y){
        inv <<- y 
    }
    
    list(set = set, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## this function needs to be called with a previously generated and object 
## made with makeCacheMatrix which was initialised with some matrix
## calling the present function the first time will calculate and output the inverse
## subsequent calls will only output the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(is.null(inv)){
        x$setInverse(solve(x$getMatrix()))
    }
    
    inv<-x$getInverse()
    inv
    
}

## makeCacheMatrix and cacheSolve functions create an object that stores a matrix and caches its inverse

## makeCacheMatrix function sets value of the matrix, gets the value of the matrix,  
## and sets the value of its inverse and gets the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
                        m<- NULL
                        set <- function(y){
                                x<<-y
                                m<<-NULL
                        }
                        get <- function() x
                        setinverse <- function(inverseSqMatrix) m <<- inverseSqMatrix
                        getinverse <- function() m
                        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve function calculates the inverse of the matrix 
## and if inverse is already calculated, it gets the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse
        if(!is.null(m)){
                message("cached data")
                return(m)
        }
        data <- x$get()
        m <- inverseSqMatrix(data, ...)
        x$setinverse(m)
        m
}

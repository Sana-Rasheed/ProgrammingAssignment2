## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL # set null value
    set <- function(y) { # set the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x  # get the value of the matrix
    setinverse <- function(inverse) inv <<- inverse # set the value of the inverse
    getinverse <- function() inv  # get the value of the inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # listing the values
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    #getting value from cache
    inv <- x$getinverse() # get value from cache
    if(!is.null(inv)) { # if not null
        message("getting cached data.") #display message
        return(inv) # return value
    }
    #value is not in cache, so calculate
    data <- x$get() # get matrix from the cache
    inv <- solve(data) # this will inverse the square matrix
    x$setinverse(inv) # set calculated value to cache
    inv # return value
}

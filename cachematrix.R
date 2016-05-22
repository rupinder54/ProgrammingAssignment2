## These functions are used to speed up time consuming computations. 
## Computed value is cached, in order to avoid re-calculations. This is done
## usind the Scoping rules in R.

## FUnction makeCacheMatrix creates a list which contains functions to 
## 1. set a matrix
## 2. get a matrix
## 3. set the inverse of matrix
## 4. get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse 
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = 
                     getinverse)
}


## Function cacheSolve calculates the inverse of matrix created using 
## makeCacheMatrix function. It first checks whether the inverse has already 
## been calculated. If it can find the inverse in the cache, it wont calculate
## it again, otherwise it calculates the inverse and stores the value in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("Getting Cached Data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

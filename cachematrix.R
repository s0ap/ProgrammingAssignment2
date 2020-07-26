## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" with a list. Steps -
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # initialize with NULL

        # set matrix value
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        # get matrix value
        get <- function() x

        # set inverse value
        setinv <- function(inverse) inv <<- inverse
        # get inverse value
        getinv <- function() inv

        # return a list with the above functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
## This function calculates the inverse of the special
## "matrix" created with the above function. Steps:
## 1. First check to see if inverse is already calculated
## 2. If yes, get inverse from cache
## 3. Otherwise, calculate the inverse and set the value of the inverse in the ## cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # check cache
        inv <- x$getinv()

        # if value is present in cache, extract it
        if(!is.null(inv)) {
                message("reading inverse from cache")
                return(inv)
        }

        # else calculate inverse
        data <- x$get()
        inv <- solve(data, ...)

        # now cache the value to use it later
        x$setinv(inv)

        # return the value
        inv
}

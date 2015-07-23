## There are two step to cache a matrix. First, use 'makeCacheMatrix'
## to create a list of functions. Although this function require a matrix
## as argument, no inversed matrix is made after the function excuted.
## It just return a list of 4 functions.
## 'cacheSolve' function will excutes these member functions in order to
## get the origin matrix, check if it was inversed (in case this is the 
## first time you run this function with the mentioned matrix, the answer
## is no), solve it, and print it
## 

# About 'makecacheMatrix':
# This function return a list that contains 4 member functions: 'set',
# 'get', 'setInv', and 'getInv'. These member functions are created to be 
# used by the 'cacheSolve' function.
# I uses <<- assignment operator in the 'set' function,
# this means that 'set' function is designed to assign value to certain variables
# outside of its environment.

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL # this is where the result of inversion is stored
        # A setter function, use this to set a matrix to object created by makeCacheMatrix function
        # e.g makeCacheMatrix(testmatrix) # here we work on testmatrix
        # makeCacheMatrix$set(testmatrix1) # here we work on testmatrix1
        set <- function(y) {
                x <<- y
                xinv <<- NULL # it also initialises xinv to null
        }
        
        get <- function() x # return the input matrix
        setInv <- function(inv) xinv <<- inv # set the inversed matrix
        getInv <- function() xinv # return the inversed matrix
        # return a list that contains these functions, so that we can use
        # makeCacheMatrix object like these
        # x <- makeCacheMatrix(testmatrix)
        # x$set(newmatrix) # to change matrix
        # x$get # to get the setted matrix
        # x$setInv # to set the inversed matrix
        # x$getInv # to get the inversed matrix
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


# About cacheSolve function:
# It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getInv() # get the inversed matrix from object x
        # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
        if(!is.null(m)) { # if the inversion result is there
                message("getting cached data")
                return(m) # return the calculated inversion
        }
        data <- x$get() # if not, we do x$get to get the matrix object
        m <- solve(data) # we solve it
        x$setInv(m) # we then set it to the object
        m # return the solved result
}

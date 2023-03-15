## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix sets a placeholder in the type of matrix which can be passed to
## function later on by using the sub-function - it's to create a matrix with
## functions six objects such as x, i, set(), get(), setinverse() and getinverse()

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## set x as a placeholder and i as NULL
        set <- function(y){   ##set() passing y to x in the parent environment 
            x <<- y
            i <<- NULL  
        }
        get <- function() x 
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        ##create a list so that function can be used such as x$get(), otherwise 
        ##error would be popped up
}


## Write a short comment describing this function
## cacheSolve passes the object created by above function as the argument 
## since the environment with the above function, the sub-functions in the above
## can be used directly, this is what lexical scoping for.
## it gets the matrix of inverse first, print out the inverse or calculate the inverse
## if it's not existing then save it into i and print i out

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
              message("getting cached inverse")
              return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

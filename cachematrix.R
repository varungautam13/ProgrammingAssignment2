## The function overall caches the values of a matrix 
## and retrives it in order to calculate its inverse 
## and cache the inverse as well, if not already done.

## makeCacheMatrix: to cache/retrive the value of matrix 
##and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL  
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set=set,
             get=get,
             getinv=getinv,
             setinv=setinv)

}


## cacheSolve: Retrieve the value of cached matrix 
## and returns its inverse and caches it as well

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}

## The following two functions are used to cache the inverse of a matrix, which may perform better than repeatedly 
## computing a Matrix inversion

## MakeCacheMatrix creates a list containing a function that:
##        * sets the value of the matrix
##        * gets the value of the matrix
##        * sets the value of the inverse of the matrix
##        * gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y)  {
          x << - y
          i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## The following returns the inverse of the matrix.  First it checks to see if the inverse has
## already been computed.  if so, it retrieves the original result.  If not, it computes the
## inverse, sets the value in the cache using setinverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("retrieving cached data")
                return(i)
        }
                data <- x$get()
                i <- solve(data)
                x$setinverse(i)
                i
}

##                                       Code    Description                                    ##
##   The first function "makeCacheMatrix" is used to create a matrix, and the second function   ##
## computes its inverse, in case it has been calculated, then the inverse matrix is retrievied  ##
##                                      from the cache.                                         ##


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The output of this function is a list of four functions: 
## I)   set the value of the matrix
## II)  get the value of the matrix
## III) set the value of the matrix
## IV)  get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}




## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

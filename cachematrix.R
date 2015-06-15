## Here I create two functions, makeCacheMatrix and cacheSolve, that cache the 
## inverse of a matrix. 
##
## makeCacheMatrix creates a special "matrix" that can cache its inverse by 
## making a list containing a funcion to
## (1) set the value of the matrix, (2) get the value of the matrix, 
## (3) set the value of the inverse, and (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set <- function(y) {
                
                x <<- y
               
                m <<- NULL
        }
        
        get <- function() x
        
        setmean <- function(mean) m <<- mean
        
        getmean <- function() m
        
        list(set = set, get = get, setmean = setmean, getmean = getmean)

}

## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatix. If the inverse has already been calcluated, and the matrix 
## has not changed, then cacheSolve retrieves the inverse from the cache
## 

cacheSolve <- function(x, ...) {
        
        m <- x$getmean()
        
        if(!is.null(m)) {
                
                message("getting cached data")
                
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setmean(m)
        
        m

        ## Return a matrix that is the inverse of 'x'
}

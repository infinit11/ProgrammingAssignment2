##  These two functions are used to create a special 
##  object that stores a matrix and cache's its inverse
##  using solve function and <<- operator as its
##  described in the example



##  In this first function you create a special matrix
##  that is really a list containing a function

makeCachematrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##  And with this function you use last one to calculate 
##  the inverse of a given matrix, as in the example, if the 
##  inverse has already been calculate it is obtained from the 
##  cache instead of do the maths, if not, it just calculate it

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
} 
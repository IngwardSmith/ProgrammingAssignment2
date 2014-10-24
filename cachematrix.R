## These two functions create 'matrix' object, allow to
## solve the inversed matrix of the 'x' matrix in it, access it and set it

## First function receives a matrix 'x' as an argument, sets the 'matrix' variable,
## that will futher contain inversed matrix, to NULL and defines four functions
## set, get, setinverse and getinverse that are returned as a list of functions
makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
    ## set function assings received matrix y to a top-environment variable x
    ## and sets its inverse 'matrix' to NULL, because it's new now.
    set <- function(y = matrix()) {
        x <<- y
        matrix <<- NULL
    }
    ## get function searches for x variable and simply returns it
    get <- function() x
    ## setinverse function assigns received argument 'inverse' to a top-level
    ## 'matrix' variable
    setinverse <- function(inverse) matrix <<- inverse
    ## getinverse searches for a 'matrix' variable and returns it
    getinverse <- function() matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve function receives a matrix x as an argument, tries to find its cache
## if possible. If not possible - it counts it and returns
cacheSolve <- function(x, ...) {    
    ## Uses $getinverse function of data 'x' to get the inversed matrix and assign
    ## it to the 'matrix' variable
    matrix <- x$getinverse()
    
    ## if it was not null - then simply returns that cached value
    if(!is.null(matrix)) {
        message("getting cached matrix")
        return(matrix)
    }
    ## get the 'x' object and assign it to 'data' variable
    data <- x$get()
    ##solve the inversed matrix and assign it to matrix variable
    matrix <- solve(data, ...)
    ## use setinverse function of 'x' to set the value of 'matrix' 
    ##variable to the inversed of x
    x$setinverse(matrix)
    
    ## Return a matrix that is the inverse of 'x'
    matrix
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #argument is a non-singular matrix
    inv_matrix <- NULL  
    set <- function(y){
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inverse) {inv_matrix <<- inverse}
    getinverse <- function() {inv_matrix}
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_matrix <- x$getinverse()
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    matrix <- x$get()
    inv_matrix <- solve(matrix, ...)
    x$setinverse(inv_matrix)
    inv_matrix
}

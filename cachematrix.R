## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function takes in a matrix and sets the inverse if it is already computed and to NULL
##if it is not computed yet. The matrix should be non-singular. 
makeCacheMatrix <- function(x = matrix()) { #argument is a non-singular matrix
    inv_matrix <- NULL  #set inverse to NULL since it has not been computed yet
    set <- function(y){
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() {x} #outputs the matrix
    setinverse <- function(inverse) {inv_matrix <<- inverse} #sets the inverse after it has been computed
    getinverse <- function() {inv_matrix}    #outputs the inverse
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function
##This function computes the inverse of the matrix if it is not already computed 
##and adds it to the attributes so that it is extracted when it is needed again.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_matrix <- x$getinverse() #extracts the inverse of the matrix
    if(!is.null(inv_matrix)) { #if the inverse has been computed already
        message("getting cached data") #lets the user know it has been computed already 
        return(inv_matrix) #outputs the inverse
    }
    matrix <- x$get() #outputs the matrix
    inv_matrix <- solve(matrix, ...)  #computes the inverse
    x$setinverse(inv_matrix) #change the inverse from NULL to the computed inverse
    inv_matrix  
}

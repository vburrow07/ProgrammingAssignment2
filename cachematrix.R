## Put comments here that give an overall description of what your
## functions do

#Matrix inversion is usually a costly computation and there may be some benefit to caching 
#the inverse of a matrix rather than compute it repeatedly (there are also alternatives 
#to matrix inversion that we will not discuss here). 
#Your assignment is to write a pair of functions that cache the inverse of a matrix.


## Write a short comment describing this function

#VB comment: makeCacheMatrix is a function that creates a special matrix object that 
#can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- solve(x)
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and 
#the matrix has not changed), then the cachesolve should retrieve the inverse 
#from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse) 
        }
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setinverse(inverse)
        inverse
}

#Test with square data matrix

solution<-makeCacheMatrix()
solution$set(matrix(4:7,2))
solution$get()

#Now try inverting this
solution$setinverse()
solution$getinverse()




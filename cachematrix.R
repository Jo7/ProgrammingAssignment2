## Caching the inverse of a matrix
### The two functions below are used to create a special object that stores a 
### matrix and caches its inverse

## The first function, makeCacheMatrix, creates a special "matrix"
### This special "matrix" is a list containting a function to: 
### 1. set the value of the matrix 
### 2. get the value of the matrix 
### 3. set the value of the inverse 
### 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) s <<- inverse
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, caceSolve, calculates the inverse of the special 
## "matrix" created above
### This function first checks to see if the inverse has already been calculated 
### and if so, gets the inverse from the cache. If not, it calculates the inverse 
### and sets the value in the cache

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}

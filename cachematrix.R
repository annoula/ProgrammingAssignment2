## This set of functions calculates the inverse of an invertible square matrix. 
## If the inverse has already been calculated previously, the inverse is cached
## in the environment from where it can be retrieved with this set of functions.
## The purpose is to save time and avoid unecessary recalculating of inverse
## matrices which for big matrices can be time-consuming.

## Description makeCacheMatrix:
## makeCacheMatrix takes a matrix as input
## m is initialized as an object where the input matrix can be saved.
## The set function assigns the input argument to x and resets m.
## The get function retrieves x from the parent environment (makeCacheMatrix 
## environment).
## The setinv function assigns "inverse" to m in the parent environment.
## The getinv function retrieves the value of m. 
## Finally, a list is created containing the four functions as elements of the 
## same name as the function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## description cacheSolve: 
## CacheSolve calculates a new inverse from the input matrix or retrieves a 
## previously calculated inverse matrix from cache.
## x$getinv() retrieves an inverse from the getinv function, if "!is.null(m)"
## is TRUE the message "getting cached data" is returned along with the actual
## inverse matrix.
## If "!is.null(m)" is FALSE cacheSolve uses the get function to retrieve the 
## input matrix (data <- x$get), calculates the inverse (m<-solve (data,...)), 
## uses the setinv function on the input object (x$setinv(m)) and prints the
## newly calculated inverse matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

## To test the set of functions you can use the following inputs: 
## a <- rbind(c(1,-0.25), c(-0.25, 1))
## a
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
##
## b <- makeCacheMatrix(a)
## cacheSolve(b)
##        [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
##
## The result of matrix multiplication (operator: %*%) of a matrix with its 
## inverse is an identity matrix, which can be used to test if the returned 
## matrix is truly the inverse. 
##
## cacheSolve(b)%*%a
## getting cached data 
##       [,1] [,2]
## [1,]    1    0
## [2,]    0    1

## [Note: the second time the cacheSolve(b) is called the
## result is retrieved from cache instead of being newly calculated]. 



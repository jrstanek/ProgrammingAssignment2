

## This function creates a special matrix which contains functions to set, get the matrix ##
## and also to set and get the inverse solution of the matrix ##

makeCacheMatrix <- function(x = numeric()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function calculates the inverse of the matrix that was created in the ##
## other function.  It first checks to see the inverse has already been calculated ## 
## if it ahs, it just grabs the inverse from the cache instead of recalculating ##
## If it hasn't been calculated, it uses the solve() function to find the inverse ##

cacheSolve <- function(x, ...) {
         i <- x$getinverse()
         if(!is.null(i)) {
                message("getting cached data")
                return(i)
         }
         data <- x$get()
         i <- solve(data, ...)
         x$setinverse(i)
         i
}


m <- matrix(c(-1,-2,1,1,2,3,-1,2,-1),3,3)
x <- makeCacheMatrix(m)

x$get()

inv <-cacheSolve(x)

inv <-cacheSolve(x)
inv
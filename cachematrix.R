#############################################################
## Provides functions create a query a cache given a matrix.
##
## Sample usage:
##   myMatrix <- matrix(rnorm(16), nrow = 4) 
##   mx <- makeCacheMatrix(myMatrix)
##   mx$get()
##   invMx <- cacheSolve(mx)
##   invMx
##   
#############################################################

##
## Creates a matrix that has the additional feature of being
## able to also return the inverse of itself.
##
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##
##  Will computate and return the inverse of the given matrix x 
##  as well as setting/caching the inverse matrix. If the inverse
##  has previously been calculated then the cached inverse will be
##  returned (as long as the matrix has not been changed since the
##  computation).
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("Getting cached inverse matrix.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

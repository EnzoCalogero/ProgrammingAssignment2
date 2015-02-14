## 
## This function defines a set of function to set get o cache 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(inv) m <<- inv
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

######################################################
## The function calculate the Inverse of the Matrix ##
######################################################
cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("The Inverse of the Matrix has been already cached ")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}

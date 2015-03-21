## makeCacheMatrix(): creates a special “matrix” object that can cache its inverse.
## cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, it’ll retrieves the inverse from the cache directly.

makeCacheMatrix <- function(x = matrix()) {
        ## x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()

        invM <- NULL
        set <- function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(inverseMatrix) invM <<- inverseMatrix
        getInverseMatrix <- function() invM
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invM <- x$getInverseMatrix()

        ## if the inverse has already been calculated, 
        ## get it from the cache
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }

        ## if not, calculate the inverse
        data <- x$get()
        invM <- solve(data, ...)

        ## set the value of the inverse to the cache
        x$setInverseMatrix(invM)
        invM

}


## With the makeCacheMatrix a list containing a function in made to 
# set and get values of the matrix.
# Also set and get is done for the values of an inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## With the function below, the inverse of the matrix is returend. A check is made 
# if the inverse has already been computed. If that is true it gets the result and skips 
# the computetion. If not true, it computes the inverse and sets the values in the cache with 
# the setinv function.

cacheSolve <- function(x, ...) {
     
		inv <- x$getinv()
		if(!is.null(inv)) {
			message("getting the cached dataset.")
			return(inv)
		}
		matrixdata <- x$get()
		inv <- solve(matrixdata)
		x$setinv(inv)
		inv
}

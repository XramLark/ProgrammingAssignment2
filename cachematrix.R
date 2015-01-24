## These functions can be used to create a special object that stores
## a matrix and chaches its inverse allowing the user to avoid the 
## repeating the costly calculation of the inverse matrix


## 'makeCacheMatrix' creates a special "matrix", which is really
## a list containing the functions to:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(mtx) inv <<- mtx
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## 'cacheSolve' calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the the
## inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the data and sets the value of the inverse
## in the cache via the `setInv` function.

cacheSolve <- function(x, ...) {
	inv <- x$getInv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInv(inv)
	## Return a matrix that is the inverse of 'x'	
	inv
}

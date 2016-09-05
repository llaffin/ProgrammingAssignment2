## These pairing of functions will create a special matrix 
## and compute it's inverse matrix, by storing the value
## in cache. 

## FUNCTION ONE
## This function creates a special matrix that:
## 1. Sets the value of the matrix
## 2. Gets the value of the matrix
## 3. Sets the value of the inverse of the matrix
## 4. Gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}

## FUNCTION TWO 
## This function solves the special matrix created in the 
## function 'makeCacheMatrix' (calculates its inverse). 
## It first checks to see if the inverse has already been
## calculated and takes that value from cache. 

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}


## Test Matrix: create a 2x2 matrix with dummy data
mat <- matrix(c(5,2,8,2), 2, 2)

## Execute functions
z <- makeCacheMatrix(mat)
cacheSolve(z) ## Successfully returns inverse of 'mat'

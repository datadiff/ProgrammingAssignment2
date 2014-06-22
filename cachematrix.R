## matrix inversion can be expensive to compute.
## caching can be used to save processing time.

## makes a cacheable inverse matrix object 
makeCacheMatrix <- function(x = matrix()) { 
	inv <- NULL

	set <- function(y) {
  	x <<- y
  	inv <<- NULL
	}
	get <- function() x
	
	setinverse <- function(inverse) { inv <<- inverse }
	
	getinverse <- function() { inv }
	
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cachable inverse matrix solver
## checks if inverted is cached and returns cached version
## otherwise computes, caches and returns
cacheSolve <- function(x, ...) {
	inv <- x$getinverse()

	if(!is.null(inv)) {
	  message("getting cached data")
	  return(inv)
	}

	data <- x$get()

	inv <- solve(data)

	x$setinverse(inv)

	inv
}



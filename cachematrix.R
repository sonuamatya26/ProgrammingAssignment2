## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The function makeCacheMatrix computes the inverse of matrix and cache it.
makeCacheMatrix <- function(x = matrix()) {
		invmatrix<-NULL
		set<-function(m1){
			x<<-m1
			invmatrix<<-NULL
		}
		get <-function() x
		setinverse<- function(solve) invmatrix <<- solve
		getinverse<- function() invmatrix
		list(set=set,get=get,
			setinverse=setinverse,
			getinverse=getinverse)
}

## Write a short comment describing this function
##This function checks whether the inverse of matrix already been calculated or not.
##If calculated then it will return the inverse from cache else computes the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invm<-x$getinverse()
	if(!is.null(invm)){
		message("getting cached data")
		return(invm)
	}
	data<-x$get()
	invm<-solve(data,...)
	x$setinvm(invm)
	invm
}
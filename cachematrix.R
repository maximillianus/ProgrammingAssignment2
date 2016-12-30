## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes in a regular square invertible matrix.
## It is able to set the matrix and retrieve it using get().
## It is also able to set the inverse and retrieve it.

makeCacheMatrix <- function(x = matrix()) {
inversematrix <- NULL
	
	##set matrix
	set <- function(matrixtobeSet)
	{
		x <<- matrixtobeSet
		inversematrix <<- NULL
	}
	##get matrix
	get <- function()x

	##set matrix inverse
	setinverse <- function(inverse)
	{
		inversematrix <<- inverse
	}

	##get matrix inverse	
	getinverse <- function() inversematrix
	
	##set type of output for this function
	list(get = get, set = set, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function checks whether inverse matrix has been calculated.
## If the inverse has been calculated, output the message,
## If it has not been calculated, calculate it, output,
## and cache in special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	##check inverse matrix is already calculated
      inversematrix <- x$getinverse()
	
	#if it has been calculated, retrieve it
	if(!is.null(inversematrix)) 
	{
   		message("getting cached data")
            return(inversematrix)
      }
	
	#otherwise, calculate the inverse of the special matrix
      matrix <- x$get()
      inversematrix <- solve(matrix)
	
	#set and cache the inverse matrix inside the special matrix
      x$setinverse(inversematrix)
	
	#also output the inverse matrix as a result to the function
      inversematrix

}

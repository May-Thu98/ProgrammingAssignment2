library(MASS)
makeCacheMatrix <- function(x = matrix()) {

inv<-NULL  # assigns NULL to a variable within the current environment 
set <- function(y)  # Save environment
{ 
	x<<-y  # cache the matrix - assigns value y from parent environment
inv<<-NULL # search through parent environments for an existing definition of the variable and set to NULL
	}
  
get<-function() x  # Get the matrix value cached with setmatrix
setinv<-function(inverse) inv<<- inverse  # Cached value of inverse matrix is saved in m
getinv<-function() {

inver<- ginv(x) 
inver%*%x
}
list (set=set, get = get, # creates list to house the four functions  
setinv = setinv,
getinv = getinv)

}

###################################
## The function "cacheSolve" returns the inverse of the matrix that is 
# returned by makeCacheMatrix function, e.g. xMat$getmatrix()
###################################

cacheSolve <- function(x, ...) 
  {

	inv<- x$getinv() # if an inverse has already been calculated this gets it
	if(!is.null(inv)){ # check to see if cacheSolve has been run before
		
    	message("getting cached data")
    
    	return(inv) 
    	}
    	# otherwise 
    data <- x$getinv() # run the getmatrix function to get the value of the input matrix
    	inv <- solve(data, ...) # compute the value of the inverse of the input matrix
    	x$setinv(inv) # run the setinverse function on the inverse to cache the inverse
    	inv # return the inverse
    	}
    	# End


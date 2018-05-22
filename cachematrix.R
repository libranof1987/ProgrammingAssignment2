# The function makeCacheMatrix creates a list containing a function to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of inverse of the matrix
# 4. Get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
	
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	get<-function()x
	setinv<-function(inverse)inv<<-inverse
	getinv<-function()inv
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


# The cacheSolve function returns the inverse of the matrix defined above. 
# 1. Check if the inverse has already been computed. If so, it gets the result and skips the computation.
# 2. If not, it computes the inverse, sets the value in the cache via setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)){
        	message("getting cached result")
        	return(inv)
        	
        }  
        data<-x$get()
        inv<-solve<-(data,...)
        x$setinv(inv)
        inv
        
      
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates the matrix object initially it will be NA of dim 1X1 if user gives the iput it will be updated
makeCacheMatrix <- function(x = matrix()) {
    inv<-matrix()
    set<- function(y){
    	x <<- y
    	inv <<- matrix()
    }
    get<- function() x
    setinv<- function(invi) inv<<- invi
    getinv<- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
    

}


## Write a short comment describing this function
## This function will get the cached data if exist or it will calculate the inverse and put it to the object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invt<- x$getinv()
        if(all(dim(x$get()) == dim(invt)) ){
        	message("getting cached data")
        	return(invt)
        }
        data<- x$get()
        inv<- solve(data)
        x$setinv(inv)
        inv
}

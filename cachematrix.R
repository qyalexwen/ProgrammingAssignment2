## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(m2){
                x <<- m2 
                inv <<- NULL 
        }        
        get <- function() x	
        setinv <- function(i) inv<<- i 
        getinv <- function() inv 
        list( set=set, get=get, setinv=setinv, getinv=getinv) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(is.null(inv)){
                m <- x$get()
                inv2 <- solve(m)
                x$setinv(inv2) 
                return(inv2)
        }else{
                message("getting cached data") 
                return(inv)
        }
}


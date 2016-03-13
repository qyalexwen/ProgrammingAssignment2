## Cache Matrix implementation, for course "R programming language", 
##  week 3 assignment


## Construct a cache matrix, with local environment bounded.
##  Returns a list of functions, includes getter and setter for matrix, 
##    and getter and setter for the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
        # initialize inversed matrix
        inv <- NULL 
        # setter 
        set <- function(m2){
                x <<- m2 
                inv <<- NULL 
        }        
        # getter 
        get <- function() x	
        # setter for inverse
        setinv <- function(i) inv<<- i 
        # getter for inverse
        getinv <- function() inv 
        list(   set=set, get=get, 
                setinv=setinv, getinv=getinv) 
}


## Caculated the inverse matrix. First calculation will be cached 
##   for later use. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(is.null(inv)){
                # If cache is empty, caculate the inverse
                m <- x$get()
                inv2 <- solve(m,...)
                x$setinv(inv2) 
                return(inv2)
        }else{  # If cache isn't empty, return the cache
                message("getting cached data") 
                return(inv)
        }
}


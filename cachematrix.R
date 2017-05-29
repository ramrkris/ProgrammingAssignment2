##This function creates a special matrix that caches its inverse.
# the first function creates a list of functions to set the value of a matrix, get the value of a matrix, set the value of the inverse of a matrix and get the value of a inverse of a matrix    
        makeCacheMatrix <- function(x = matrix()) {         
                inv <- NULL        
                set <- function(y) {          
                        x <<- y          
                        inv <<- NULL        
                }        
                get <- function() x        
                setinverse <- function(solve) inv <<- solve        
                getinverse <- function() inv        
                list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)    
        } 
## the second function uses the list created by previous function and caches the inverse of a matrix if available or solves for inverse     
        cacheSolve <- function(x, ...) {         
                inv <- x$getinverse()        
                if(!is.null(inv)) {          
                        message("getting cached data")          
                        return(inv)        }        
                data <- x$get()        
                inv <- solve(data, ...)        
                x$setinverse(inv)        
                inv    
        } 


## The function 'makeCacheMatrix()' returns a list containing four functions in which information can be cached (see below) 
## The function 'cacheSolve()' either retrieves the cached inverse of a matrix from the functions returned by 'makeCacheMatrix()'
## or, if a cached value does not exist yet, caches it in the functions returned by 'makeCacheMatrix()'

# The 'makeCacheMatrix' function returns a list containing four functions: 'set()', 'get()', 'setinv()' and 'getinv()':
# 1. The 'set()' function superassigns its argument y to the matrix argument 'x' and also superassigns the NULL object (undefined) to the variable m
#    I found the 'set()' function not relevant for the current assignment. However, I still defined it to stick as close to the example function as possible
# 2. The 'get()' function returns the original matrix argument ('x') of the 'makeCacheMatrix function()'
# 3. The 'setinv()' function calculates the inverse of its input matrix and superassigns it to 'm'
# 4. The 'getinv()' function returns the value of 'm'

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL		#'m' is assigned the NULL value (i.e. not defined)
            set <- function(y) {			
                    x <<- y		
                    m <<- NULL		
            }
            get <- function() x				
            setinv <- function(solve) m <<- solve	
            getinv <- function() m		
            list(set = set, get = get,			
                 setinv = setinv,			
                 getinv = getinv)

}


## The cacheSolve function either calculates the inverse of the input matrix, caches and returns it, or alternatively,
## if this information is already cached, retrieves it from the functions created by 'makeCacheMatrix()' and returns it

cacheSolve <- function(x, ...) {
	m <- x$getinv()					   # here, 'm' is assigned the value stored by the 'getinv()' function of 'x' from the 'makeCacheMatrix()' function
            if(!is.null(m)) {			# if 'm' is defined, retrieve its cached value, return it, print message, then end the function 
                    message("getting cached data")
                    return(m)
            }		                        # if 'm' is not defined:				
            data <- x$get()				 			# - assign 'data' the value of the 'get()' function of 'x' from the 'makeCacheMatrix()' function
            m <- solve(data, ...)	      # - assigm 'm' the inverse of the 'data' object  	
            x$setinv(m)                 # - use 'm' as argument of the 'setinv()' function of 'x' from the 'makeCacheMatrix()' function
            m		                        # - return 'm'
	
}

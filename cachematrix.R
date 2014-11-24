##The following programs will take an invertible matrix and return its inverse.  If the inverse has not been calculated before, these functions will calculate and store it;
##if it has been calculated previously, the functions will locate and return the stored inverse.

## makeCacheMatrix will create a special "matrix," which is a list that contains funtions to set and retrieve the value of the matrix and to set and retrieve the value of the inverse.


makeCacheMatrix <- function(x = matrix()) {			#the input is an invertible matrix (non-invertible matrix will return an error)
        inv <- NULL							#define 'inv' to be used later as the inverse of x matrix (will be reset to NULL every time makeCacheMatrix is called)
        set <- function(y) {						#define 'set' as a function with matrix as input
                x <<- y							#search for x in parent environment and redefine as equal to y; if x doesn't alreadyexist, define globally as y
                inv <<- NULL						#reset the inverse 'inv' as NULL
   }
        get <- function() x						#defines a function that will return the original matrix when called
        setinverse <- function(inverse) inv <<- inverse	#defines a function where the input will be the inverse of matrix x and which will set 'inv' equal to that inverse; will be called by cachesolve during first access of cachesolve and will store the value
        getinverse <- function() inv				#defines a function that will return the inverse 'inv' when called; will return the cached inverse value on subsequent accesses of cachesolve
        list(set = set, get = get,					#return a list of the functions defined above to be used in cacheSolve
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created by makeCacheMatrix.  It first checks to see if the inverse has already been calculated; if so,
## it does not recalculate it, but it retrieves the stored inverse and returns it.  If the inverse has not been calculated, it calculates it, stores it in the cache,
## and returns it.

cacheSolve <- function(x, ...) {			#the input is an object created by makeCacheMatrix
        inv <- x$getinverse()				#accesses the object 'x' and gets the value of the inverse
        if(!is.null(inv)) {				#if the inverse was already cached (not NULL)...
                message("getting cached data")  #...print this message in the console
                return(inv)				#...and print the inverse and end the function
        }
        data <- x$get()					#this step is used only if x$getinverse() was NULL (i.e. inverse was not cached); it stores the original input matrix x in 'data'
        inv <- solve(data, ...)			#calculate the inverse of the input matrix
        x$setinverse(inv)				#store the calculated inverse in x
        inv	
}

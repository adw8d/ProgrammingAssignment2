## These two functions work together to allow for the
## inverse of a matrix (using solve()) to be calculated 
## once, stored as a cached value, and then retrieved 
## again without having to recalculate the inverse again.

## the makeCacheMatrix function both stores a matrix "x", and 
## creates a series of other functions which are used to 
## set the value of the matrix again, get the value of the 
## matrix, set the inverse of the matrix, and get the inverse
## of the matrix. This function needs to be used in conjunction
## with the cachesolve() function to actually calculate the inverse
## of the matrix "x".

makeCacheMatrix <- function(x = matrix()) { 
        
        ## Create a function makeCacheMatrix() that 
        ## takes a matrix-class object x as its argument.
        ## NOTE: I have learned that you must use the matrix() function
        ## as part of the argument to avoid error.
        
        z <- NULL 
        
        ## sets the value of the matrix inverse, "z", to NULL
        ## since it hasn't been calculated yet
        
        set <- function(y) { 
                
                ## creates a function called "set" which takes 
                ## y as its argument
                
                x <<- y 
                
                ## whatever we assign to the "y" in set()
                ## function will replace the "x" that 
                ## exists in the original makeCacheMatrix()
                ## function
                
                z <<- NULL
                
                ## z gets assigned to null again
                ## because it will necessarily change if the 
                ## value of matrix "x" changes
                
        }
        
        get <- function() x
        
        ## creates a get() function that returns the value of 
        ## x, the original matrix (or the matrix that was
        ## set later using "set()")
        
        setinverse <- function(inverse) z <<- inverse
        
        ## creates a function called "setinverse" that takes as its
        ## argument a variable called "inverse", which it then 
        ## assigns to the parent environment "z" value        
        
        getinverse <- function() z
        
        ## creates a function called "getinverse" that returns the
        ## value of "z" - the inverse of the parent function        
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        ## returns a list that says "set, get, setinverse, getinverse"...
        ## not sure why.
}

## cachesolve() is used in place of the solve() function to either calculate
## the inverse of a matrix x, or to retrieve (and then store) the value of the 
## inverse of matrix x if it has already been stored using the above 
## makeCacheMatrix() function.

cachesolve <- function(x, ...) {
        
        ## creates a function cachesolve() that takes x as its argument
        
        z <- x$getinverse()
        
        ## assigns the value of the $getinverse() function to z
        
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
                
                ## if z is not NULL, then return the value of z...
                ## this is the inverse of x
        }
        
        matrix <- x$get()
        
        ## assign the matrix "x" to a variable called "matrix" using the
        ## makeCacheMatrix() subfunction get()
        
        z <- solve(matrix, ...)
        
        ## assign to "z" the inverse of the "matrix" matrix
        
        x$setinverse(z)
        
        ## set the inverse of the matrix (now named "z") to the "z" variable
        ## in the original environment of the makeCacheMatrix() function
        
        z
        
        ## return the value of z
}

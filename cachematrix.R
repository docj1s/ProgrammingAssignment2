## Creating a cached matrix and computing its inverse 
##
## Code follows the structrue of the makeVector and cacheMean functions. 

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
  ## x: must be a square matrix that has an inverse
  ## return: a list of four functions that can be used by cacheSolve

makeCacheMatrix <- function(x = matrix()) 
{
        matInv <- NULL #initialize object
        set <- function(y) ## setter for the matrix
        {
            x <<- y #cache assignment of the input argument to x in the parent environment
            matInv <<- NULL #cache assignment of matInv in the parent environment to null
                            #clearing any value of matInv that had been cached previously
        }
        
        get <- function() x ## getter for the matrix
        
        setInv <- function(inv) matInv <<- inv #defines the setter behavior for the inverse
        getInv <- function() matInv #defines the getter behavior for the inverse
        
        list(set=set, get= get, setInv = setInv, getInv = getInv) #creates an object
                                                                  #of the functions 
}


## cacheSolve: computes the inverse matrix
  ## x: output of makeCacheMatrix()
  ## return: inverse of the original matrix that was inputted to makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matInv <- x$getInv()  #attempts to retrieve the inverse
        
        if(!is.null(matInv))  #use the cached value if not null
        {
            return (matInv)
        }
        
        #otherwise recompute inverse
        data <- x$get()
        matInv <- solve(data)
        x$setInv(matInv)
        matInv
}

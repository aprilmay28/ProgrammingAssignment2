## Module 2, Week 3 Assignment


makeCacheMatrix <- function(x = matrix()) 
{  ## set matrix to x
        orig <- NULL
        set <- function(y) ## to set then get the original matrix
        {
            x <<- y
            orig <<- NULL
        }
        get <- function() x
        setNewinverse <- function(Newinverse) ## to set then get the inversed matrix
             Newinverse <<- Newinv
        getNewinverse <- function() Newinv
        list (set = set,
              get = get,
              setNewinverse = setNewinverse,
              getNewinverse = getNewinverse)
}


cacheSolve <- function(x, ...) 
        ## Return a matrix that is the inverse of 'x'
{ ## display inverse of orig matrix
        Newinverse = x$getNewinverse()
        ## if calculation done, then
        if (!is.null(Newinverse))
                { ## display from cache
                message("getting cached data")
                return(Newinverse)
                }
        ## if inverse not yet calculated
        mat.data = x$get()
        Newinverse = solve(mat.data, ...)
        
        x$setNewinverse(Newinverse)
        return(Newinverse)                      
}

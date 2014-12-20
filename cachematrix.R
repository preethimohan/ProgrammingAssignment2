## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
    m<-NULL
    set <- function(y)
    {
        x<<-y
        m<<-NULL
    }
    get <- function() x
    setmatrix <- function(solve) m<<-solve
    getmatrix <- function() m
    list(get = get, set=set, getmatrix = getmatrix, setmatrix = setmatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
    m <- x$getmatrix()
    if(!is.null(m))
    {
        message("Getting cached data")
        return(m)
    }
    matrix <- x$get()
    ## Finding inverse of the matrix.
    
    m <- solve(matrix,...)
    x$setmatrix(m)
    m
}
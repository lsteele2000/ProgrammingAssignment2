
## cachematrix.R
#   Naive implementaion of a matrix/matrix inverse caching object.

#   Note: The implementation is per the project instructions though it can be
#   argued that setinv should not be exposed and the cache object itself should
#   be responsible for solving/caching it's inverse within the getinv function. 
#


# makeCacheMatrix -- factory function for creating a matrix inverse cache.
#   Returns a list of named functions for getting/setting of a matrix and it's
#   inverse. The matrix/inverse are held as a closure associated with the 
#   returned list.
makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(new_m) {
            m <<- new_m
            inv <<- NULL
    }
    get <- function() m
    setinv <- function(m_inv) inv <<- m_inv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## cacheSolve -- returns the inverse of the matrix held in the x parameter's
#   environment. If the inverse had previously been calculated and cached
#   in x's that value is returned. Other the inverse will be calculated and
#   cached in x.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}

## test_CacheSolve -- minimalist unit test for CacheSolve/makeCacheMatrix
test_CacheSolve <- function() {
  m <- matrix( c( 1, -2, -3, 0,1,0, 0,0,1),nrow=3 )
  cm <- makeCacheMatrix( m )
  cm1 <- cm$get()
  if ( !all.equal( m, cm1 ) ) {
    simpleError( "get on CacheMatrix did not match original matrix")
  }
  inv <- cm$getinv()
  if ( !is.null(inv)) {
    simpleError( "expected NULL from getinv on CacheMatrix prior to cacheSolve call")
  }
  inv <-  cacheSolve(cm)
  cachedInv = cm$getinv()
  if ( !all.equal( inv, cachedInv ) ) {
    simpleError("getinv() return value differs from cacheSolve return value")
  }
  cm$set( t(m) )
  inv <- cm$getinv()
  if ( !is.null(inv) ) {
    simpleError( "set of new matrix on CacheMatrix did not null the cache")
  }
  message( "OK")
}

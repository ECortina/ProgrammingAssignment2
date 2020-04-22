

## This function create a cache matrix that its inversed > cachemake

cacheMake <- function(x = matrix()) {
    invt <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) invt <<- inverse
    getInv <- function() invt
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This one recive the cache > cachesolve

cacheSolve <- function(x, ...) {
    invt <- x$getInv()
    if (!is.null(invt)) {
        message("getting cache data")
        return(invt)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInv(inv)
    inv
}

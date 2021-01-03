## The below functions will calculate the inverse of a matrix if it hasn't already
## been calculated. if it has already been calculated, it will get the cached solution

## the makeCacheMatrix function makes a holder "vector" for the solved matrix
makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  set_invmat <- function(solve) inv_mat <<- solve
  get_invmat <- function() inv_mat
  list(set = set, get = get,
       set_invmat = set_invmat,
       get_invmat = get_invmat)
}


## cacheSolve function solves the matrix (gets the inverse) if that solution is
## not already present in the holder vector. if it's already present, then it uses
## the already-saved/cachced solution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$get_invmat()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$set_invmat(inv_mat)
  inv_mat
}

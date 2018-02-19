## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         inv_mat <- NULL
       set <- function(y) {
              x <<- y
              inv_mat <<- NULL
       }
       get <- function() x
       set_inv <- function(solve) inv_mat <<- solve
       get_inv <- function() inv_mat
       list(set = set, get = get,
            set_inv =   set_inv ,
            get_inv = get_inv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv_mat <- x$get_inv()
       if(!is.null(inv_mat)) {
              message("getting cached data")
              return(inv_mat)
       }
       data <- x$get()
       inv_mat <- solve(data, ...)
       x$set_inv(inv_mat)
       inv_mat
}

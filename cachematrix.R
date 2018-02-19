## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix function takes in an (invertible) matrix and save the matrix to cache
##creates a list of the differnet paramenters related to the function  

makeCacheMatrix <- function(x = matrix()) {
         inv_mat <- NULL
       set <- function(y) {                           ###
              x <<- y
              inv_mat <<- NULL
       }
       get <- function() x                            ###list item that will return the original matrix
       set_inv <- function(solve) inv_mat <<- solve   ### sets the function used i.e solve
       get_inv <- function() inv_mat                  ### returns the inverted matrix once created
       list(set = set, get = get,
            set_inv =   set_inv ,
            get_inv = get_inv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv_mat <- x$get_inv()                       ### takes the original matrix from makeCacheMatrix list
       if(!is.null(inv_mat)) {                       ### checks to see if inverse has already been created and cached 
              message("getting cached data")           ##and returns that matrix if it has
              return(inv_mat)
       }
       data <- x$get()                                ### this part takes the cacheMatrix and applys the solve() function to it
       inv_mat <- solve(data, ...)                    ### creates the inverse matrix and returns it to $set_mat of the makeCacheMatrix list
       x$set_inv(inv_mat)
       inv_mat
}

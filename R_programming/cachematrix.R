## Write functions to cache potentially time-consuming computations. ie. if the contents of a matrix
## are not changing, it is more sensible to cache the value so that when needed it can be looked up 
## in the cache rather than recomputed.

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    matrix_inverse <- NULL
    set_matrix <- function(y) {
        x <<- y
        matrix_inverse <<- NULL
    }
    get_matrix <- function() x
    
    set_matrix_inverse <- function(inverse) matrix_inverse <<- inverse
    get_matrix_inverse <- function() matrix_inverse
    
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_matrix_inverse = set_matrix_inverse,
         get_matrix_inverse = get_matrix_inverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with the makeCacheMatrix function. 
## it first checks if the inverse has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the set_matrix_inverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrix_inverse <- x$get_matrix_inverse()
    if(!is.null(matrix_inverse)) {
        message("getting cached data")
        return(matrix_inverse)
    }
    data <- x$get_matrix()
    matrix_inverse <- solve(data)
    x$set_matrix_inverse(matrix_inverse)
    matrix_inverse
}

## The following code contains 2 functions
## 1. makeCacheMatrix
## 2. cacheSolve
## that cache the inverse of a matrix


## makeCacheMatrix takes an invertible matrix 
## - a square matrix with non-zero determinant - as 
## its input argument and creates a list of functions to
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the inverse of the matrix (set_inverse)
## 4. get the inverse of the matrix (get_inverse)
makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){
                x
        }
        set_inverse <- function(inverse){
                inv <<- inverse
        }
        get_inverse <- function(){
                inv
        }
        list(set = set, get = get, 
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## cacheSolve takes as its input argument a special object
## created by the makeCacheMatrix above.
## It first checks if the inverse of the matrix has already been
## calculated "!is.null(inv)".
## - If it has, then it RETRIEVES the inverse inv from the cache
## without computation.
## - Otherwise, it CALCULATES the inverse of the matrix data using
## the solve() function, and sets the value of the inverse in the
## cache via the set_inverse() function
cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
        if(!is.null(inv)){
                message("Getting cached inverse...")
                return(inv)
        }
        message("Calculating the inverse...")
        matrix_data <- x$get()
        inv <- solve(matrix_data, ...)
        x$set_inverse(inv)
        inv
}
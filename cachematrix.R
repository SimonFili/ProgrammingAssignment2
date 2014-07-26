# First function creates an "envelope" with the matrix called in parameter
# to calculate the inversed matrix and save it in this envelope. It's 3 function listed

makeCacheMatrix <- function(x = matrix())
{
    m <- NULL

    ## First function is called to set the inversed matrix
    set_solved <- function(var1) {
        m <<- var1
    }
    
    ## Second function return the inversed matrix or NULL if the one is not available
    get_solved <- function() {
        m
    }
        
    ## Third function returns x which is available from the 
    ## scope of the parent functions due to lexical scoping
    get_matrix <- function() {
        x
    }
    
    
    ## Return an envelope as a list containing all functions defined above plus the original matrix
    list(get_matrix = get_matrix,
         set_solved = set_solved,
         get_solved = get_solved)
    
}

##
# This function calculates the inverse matrix for a given
# source matrix and caches the result for the future use.
#
# It accepts an envelope of the list type which contains
# the source matrix and the utility functions which are used
# to get and save the inversed matrix.
#
# Before the calculation it checks whether there is the
# inverse matrix for the given source matrix in the
# envelope.
#
# If the inverse matrix is not available it calculates the
# one and puts it into the envelope and returns the result.
##
cacheSolve <- function(x) {
    m <- x$get_solved()
    
    if( !is.null(m) )
    {
        message("Returning cached data")
        return(m)
    }
    
    message("Cached inverse matrix not present, calculating it")
    
    data <- x$get_matrix()
    
    inverse_matrix <- solve(data)
    x$set_solved(inverse_matrix)
    inverse_matrix
}


## Now we test if cacheSolve() works by create a 100x100 matrix (x)
## using stats package and dim
## then calling makeCacheMatrix fist to get the list of functions
## then we compute first to create the cache
## then we compute again, we should read the cache
## and finaly compares computuded with the result of solve

nrows <- 100
ncols <- 100
x <- stats::rnorm(nrows*ncols)
dim(x) <- c(nrows, ncols)

y <- makeCacheMatrix(x)
compute_with_caching_solve <- cacheSolve(y) # should say no cache on first run
compute_with_caching_solve <- cacheSolve(y) # should say, getting cache
normal_compute_with_solve <- solve(x)       # solving with normal solve function
identical(compute_with_caching_solve, normal_compute_with_solve) # Check if identical

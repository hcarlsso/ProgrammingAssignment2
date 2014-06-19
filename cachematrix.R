## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## Function to create the cache for the matrices inverse
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    # When solved is called without any second arguments, 
    # it calculates the inverse
    m <- solve(data, ...)
    x$setinv(m)
    m
}

native_inverse  <- function(iter,matrix){
    
    for(i in seq(0,iter)){
            
        solve(matrix) 
    }
}

# Test functionality
# Use random numbers, infinite small change the matrix is not invertable
matrix_to_solve  <- matrix(rnorm(100),10,10)
matrix_solved_native  <- solve(matrix_to_solve)
matrix_solved_cache  <- cacheSolve(makeCacheMatrix(matrix_to_solve))
difference  <- matrix_solved_native - matrix_solved_cache
difference
# solve(matrix)
iterations  <- 1E5

system.time(replicate( iterations, cacheSolve(makeCacheMatrix(matrix_to_solve))))
system.time(native_inverse(iterations,matrix_to_solve))
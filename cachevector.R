makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

native_mean  <- function(iter,big_number){
    
    for(i in seq(0,iter)){
        
        mean(seq(0,100000)) 
    }
}
iterations  <- 1E4
big_number  <- 1E3
system.time(replicate( iterations, cachemean(makeVector(seq(0,big_number)))))
system.time(native_mean(iterations,big_number))
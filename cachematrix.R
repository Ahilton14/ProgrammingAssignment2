# I write the input x as an array, after that I put the value "a" as null.
# Then I see the relation that they give as an example in coursera and I 
# change the reference "mean" to "solve" to obtain what they ask me.

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) a <<- solve
        getsolve <- function() a
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        a <- x$getsolve()
        if(!is.null(a)) {
                message("getting inversed matrix")
                return(a)
        }
        data <- x$get()
        a <- solve(data, ...)
        x$setsolve(a)
        a
        ## Return a matrix that is the inverse of 'x'
}

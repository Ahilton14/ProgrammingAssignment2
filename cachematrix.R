# I write the input x as an array, after that I put the value "a" as null.
# Then I see the relation that they give as an example in coursera and I 
# change the reference "mean" to "solve" to obtain what they ask me.

makeCacheMatrix <- function(x = matrix()) {   #Creating the matrix function
        a <- NULL                             #Put "a" to start the reverse property
        set <- function(y) {    #here I configure the matrix
                x <<- y
                a <<- NULL
        }
        get <- function() x     # Return the matrix
        setsolve <- function(solve) a <<- solve
        getsolve <- function() a        #Inverse property
        list(set = set, get = get,      #Defined list
             setsolve = setsolve,
             getsolve = getsolve)
}


#The variable "cacheolve" obtains the inverse of the matrix that throws "MakeCacheMatrix" and 
#the following is added: If the inverse is already calculated, and the array array has not changed, 
#then the variable "cachesolve" retrieves the reverse matrix of the cache.

cacheSolve <- function(x, ...) {
        a <- x$getsolve()
        if(!is.null(a)) {
                message("getting inversed matrix")   #Otherwise, the written message is included in quotation marks.
                return(a)
        }
        data <- x$get()         #The matrix of my variable is obtained
        a <- solve(data, ...)   #The inverse is obtained by multiplying matrices
        x$setsolve(a)           #the inverse of the variable is obtained
        a                       #Returns the variables, in our case a matrix
        
}

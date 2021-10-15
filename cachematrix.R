# I write the input x as an array, after that I put the value "a" as null.
# Then I see the relation that they give as an example in coursera and I 
# change the reference "mean" to "solve" to obtain what they ask me.

makeCacheMatrix <- function(x = matrix()) {   #Creating the matrix function
        a <- NULL                             #Put "a" to start the reverse property
        set <- function(y) {    #here I configure the matrix
                x <<- y
                a <<- NULL
        }
        get <- function() x     #To get the matrix and let me
        setsolve <- function(solve) a <<- solve #This is how the inverse of the matrix is obtained
        getsolve <- function() a        #You get the value of the function
        list(set = set, get = get,      #Defined list
             setsolve = setsolve,
             getsolve = getsolve)
}


#The variable "cacheolve" obtains the inverse of the matrix that throws "MakeCacheMatrix" and 
#the following is added: If the inverse is already calculated, and the array array has not changed, 
#then the variable "cachesolve" retrieves the reverse matrix of the cache.

cacheSolve <- function(x, ...) {        #Here the reverse matrix of "X" is returned
        a <- x$getsolve()         #It is verified if the function for the given matrix has already been calculated
        if(!is.null(a)) {
                message("getting inversed matrix")   #In case it has already been calculated, the message is thrown in quotes
                return(a)               #returns the value of the function
        }
        data <- x$get()         #The matrix of my variable is obtained
        a <- solve(data, ...)   #The inverse is obtained by multiplying matrices
        x$setsolve(a)           #the inverse of the variable is obtained
        a                       #Returns the variables, in our case a matrix
        
}

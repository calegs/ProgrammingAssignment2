
# First, I go through the steps explained in the article Demistifying makeVector(). Have adapted verbiage step by step, always trying to express the step with accuracy

# The makeCacheMatrix() function

        # 1. Initialize Objects

makeCacheMatrix <- function(x = matrix()) { #makeCache Matrix initializes 2 objects, x and i, and defines x as an empty matrix
        i <- NULL # inverse square matrix set to NULL, initializing it as an object within the makeCacheMatrix environment, for later use
        
        # 2. Define the "behaviors" or functions for objects of type makeCacheMatrix()
        
        set <- function(y) {  # makeCacheMatrix defines de set() function, that takes and argument named y
                x <<- y # the <<- operator assigns the value on the right side to the object on the parent environment, x
                i <<- NULL # clears any value of i that has been chached by a prior execution of cacheSolve() 
        }
        
        # 3. Create a new object by returning a list()
        
        get <- function() x # symbol x is not defined whithin get(). It is retrieved from the parent environment
        setmatrix <- function(solve) i <<- solve # i is defined in the parent env. and needs to be accessed after setmatrix() completes. The <<- operator assigns the input value to the value of i in the parent env. 
        getmatrix <- function() i # just like the getter for x, R uses lexical scoping to find the correct symbol i to retrieve its value
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix) # assigns each of these functions a named element within a list, returning it to the parent env. 
}


# The cacheSolve() function

# The cacheSolve() function will require an input type makeCacheMatrix. Using a regular matrix as input for cacheSolve will generate an error, as the $ form of extract operator does not work with atomic vectors.

cacheSolve <- function(x, ...) { # starts with a single argument and an elipsis, allowing the caller to pass aditional arguments into the function
        i <- x$getmatrix() # function attempts to retrieve a matrix from the object passed in as an argument
        if(!is.null(i)) { # then it checks if the result is NULL.
                message("getting cached data")
                return(i) # if value is not null, we have a valid, cached inverse matrix and can return it to the parent env. 
        }
        data <- x$get() # if value is null, cacheSolve() gets the matrix from the input object
        i <- solve(data, ...) # calculates the inverse matrix
        x$setmatrix(i) # uses the setmatrix() function on the input object to set the inverse matrix in the input object 
        i # and returns the value of the inverse square matrix to the parent environment by printing it 
}


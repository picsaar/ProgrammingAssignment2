## This function inverses the matrix.  
## It uses the caching technique.  If the inverse is already exists, it just return the matrix. 
## If not it inverses the matrix using solve and return the inverse matrix and stores the matrix in memory.
## For subsequent method calls, it just returns the cached data.

## This function has a set of get,set and getmatrix,setmatrix methods.   
## The getters are used to get the values stored using setters.
## The setter are used to set the values.
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function takes makeCacheMatrix as input.  If the matrix is already inversed, then it simply returns the cached data.  
## If not it uses solve function to inverse the matrix and calls the parent function accordingly to get the input matrix and 
## store the inverse matrix after.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}


# Function makeCacheMatrix creates a special "matrix", 
#which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of the matrix
#get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list ( set = set, get = get, setinverse = setinverse,
               getinverse = getinverse)
}


#Function "cacheinverse" calculates the inverse of the special "matrix" created 
#with the above function "makeCacheMatrix". However, it first checks to see if 
#the inverse has already been calculated. If so, it gets the inverse from the cache 
#and skips the computation. Otherwise, it calculates the inverse of the data stored 
#in Matrix and sets the value of the inverse in the cache via the setinverse function.

cacheinverse <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

#n1 <- matrix(c(6, 2, 8, 4), nrow = 2, ncol = 2)
#n1

#cachedmatrix <- makeCacheMatrix(n1)
#cacheinverse(cachedmatrix)

#I1 <- matrix(c(0.5, -0.25, -1, 0.75), nrow = 2, ncol = 2)
#I1

#cachedmatrix <- makeCacheMatrix(I1)
#cacheinverse(cachedmatrix)
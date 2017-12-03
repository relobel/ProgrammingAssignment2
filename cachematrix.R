## Inverse Matrix with cache management 
## R.Lobel - R Programming - Week 3 - RAssignment 2- Lexical Scoping 

# The program have 2 functions: `makeCacheMatrix` and `cacheSolve`
# makeCacheMatrix - Creates a matrix object, keeping in its environment the matrix and its inverse, 
# caching the inverse, avoids inverting it again, which is very time consuming.
# cacheSolve - Inverts a given Matrix. If the inverse is in Cache it gets the inverse from cache

makeCacheMatrix <- function(x = matrix()) {
# Creates an object of type makeCacheMatrix, which holds two variables "x" & "m", 
# where "x" stores the original matrix and "m" (initialy null) caches the matrix inverse
# this object also holds 4 functions on its environment: set, get, setinv & getinv 

m <- NULL  # on creating the makeCacheMatrix object it clears (NULL) the cache "m"

set <- function(y) {              #sets "x" with the matrix and cleans the cache "m"
    x <<- y
    m <<- NULL
}
get <- function() x               # returns the stored matrix
setinv <- function(inv) m <<- inv # receives the inverted matrix and stores it in the cache "m"
getinv <- function() m            # returns the cached inverted matrix "m"         

list(set=set, get=get, setinv=setinv, getinv=getinv) 
# MakeCacheMatrix returns a list with the 4 functions 
# (set, get, setinv & getinv) to the parent environment
# by naming each element of the list by the function name, 
# allows the 4 functions to be called as object$function in the cacheSolve function
}

cacheSolve <- function(x, ...) {
# Returns a matrix that is the inverse of 'x'
# if the inverse is already in the cache it gets the value from the cache 
# avoiding doing the inverse, which is a very time consuming function
m <- x$getinv()                         # gets the value stored in the cache set MakeCacheMatrix
  if(!is.null(m)) {                     # if the content of the cache is not NULL
    message("getting cached data")      # it posts a message that a valid cache was found
    return(m)                           # and returns the cache, avoiding inverting the matrix again
  }
  data <- x$get()               # if the cache is empty, it gets the matrix stored by MakeCacheMatrix
  m <- solve(data, ...)         # then calculates the inverse of the matrix
  x$setinv(m)                   # then stores the inverse in the cache provided by MakeCacheMatrix
  m                             # and returns this just calculated inverse matrix
}

## example of use of makeCacheMatrix / cacheSolve

# creates the cache object (create the 4 functions, m=Null & x=argument)
#> a <- makeCacheMatrix(matrix(c(1,-1/4,-1/4,1),2,2))
#> a$get()
# [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00

#> a$getinv()
# NULL

# calling cacheSolve for the first time, inverting the matrix and storing it in the cache

#> cacheSolve(a)
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667

# checking if the matrix is really the inverse
#> a$get() %*% a$getinv()
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1

# calling the cachesolve again for the same matrix luc, to check if gets the inv from cache

#> cacheSolve(a)
# getting cached data
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667

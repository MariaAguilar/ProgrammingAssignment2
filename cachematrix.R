# The 'makeCacheMatrix' function and the 'cacheSolve' function 
# are able to cache potentially time-consuming computations.


# This function creates a special "matrix" object that can cache its inverse.
# This function returns a list of functions used by 'cacheSolve' function.

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    
    # This function creates a special matrix.
    set<-function(y){
        x<<-y
        i<<-NULL
    }
    
    # This function gets the matrix created previously.
    get<-function()x
    
    # This function inverts the matrix.
    setInv<-function(inverse)i<<-inverse
    
    # This function gets the matrix's inverse.
    getInv<-function()i

    # The value returned is a list.
    list(set=set,get=get,setInv=setInv,getInv=getInv)
}


# This function computes the matrix's inverse returned by makeCacheMatrix.
# If the inverse has already been calculated, 
# then this function should retrieve the inverse from the cache.
# Suppose that the matrix supplied is always invertible and square.

cacheSolve <- function(x, ...) {
    # It attempts to get the matrix's inverse
    i<-x$getInv()

    # It checks to see if the matrix's inverse has been calculated 
    # else it calculates it.    
    if(!is.null(i)){
        message("getting cached data!")
        return(i)
    }
    
    data<-x$get()

    i<-solve(data,...)

    x$setInv(i)

    # The value returned is the matrix's inverse.
    i
}


# Here is an example of the functions calls:

# The object 'a' will store the result of 'makeCacheMatrix'.
# a<-makeCacheMatrix()

# Then, from the returned list, it extracts the function 'set', 
# and we provide to it a matrix.
# a$set(matrix(c(2,-2,-5,-1,3,1,0,4,6),3,3))

# It will return the matrix's inverse.
# cacheSolve(a)

# The result looks like this:
#            [,1]       [,2]       [,3]
# [1,]  0.3888889 0.16666667 -0.1111111
# [2,] -0.2222222 0.33333333 -0.2222222
# [3,]  0.3611111 0.08333333  0.1111111


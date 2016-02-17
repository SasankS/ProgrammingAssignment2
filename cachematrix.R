## makeCacheMatrix creates a special "matrix", which is really a list containing a function to

#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
     i<-NULL
     set<-function(y){##For Setting the value of the matrix
       x <<- y
       i<-NULL
     }
     get<-function() x ##For Getting the value of the matrix
     setInverse <- function(inverse) i <<- inverse  ##For Setting value of inverse Matrix
     getInverse <- function() i ## For Getting value of inverse Matrix
     list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}

## cacheSolve function calculates the inverseMatrix of the special "matrix" created with makeCacheMatrix function.
## However, it first checks to see if the inverseMatrix has already been calculated.
## If so, it gets the inverseMatrix from the cache using getInverseMatrix and skips the computation. 
##Otherwise, it calculates the inverseMatrix of the matrix and sets the value of the inverseMatrix in the cache via the setInverseMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<- x$getInverse()  ##Getting the value of the inverse matrix
        if(!is.null(i)){
          message("Getting Cache inverse matrix")
          return(i)
        }
        d<- x$get() ##For Getting the value of the matrix
        i<- solve(d,tol = 1e-20) ##Setting the tolerance low to accomodate smaller numbers in the matrix
        x$setInverse(i) ##For Setting value of inverse Matrix so that it can be cached and used next time
        i
}

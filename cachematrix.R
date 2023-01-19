## On the website cran.r-project provides the inverse function is available for
## solving the inverse of the matrix.
## In first place, we need to install the matlib package matlib and load the
## library(matlib)
## 
## install.packages("matlib")
## library(matlib)

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) im <<- inv # This function will turn   
        getinverse <- function() im                  # our matrix into a inverse
        list(set = set, get = get,                   # matrix.
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## This function computes the inverse of matrix stored in the previous function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if(!is.null(im)){
                message("getting cached data")
                return(im)
        }
        inv_matrix <- x$get()
        im <- solve(inv_matrix, ...) # the SOLVE function is responsible for 
        x$setinverse(im)             # calculating the inverse matrix
        im
        
        
        
}

##  Thanks and acknowlegment in solving this assignment to: 
##  https://cran.r-project.org/web/packages/matlib/vignettes/inv-ex1.html, 
##  https://rstudio-pubs-static.s3.amazonaws.com/222005_c0d14ba068ed4bb0b6a7f61892882273.html
##  And https://www.geeksforgeeks.org/inverse-of-matrix-in-r/. God bless you!

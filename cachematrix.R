## Our first function creates an array and 
## stores its inverse in the cache while
## our second function will start from the first 
## since it will use the created array
## o calculate its inverse, in case it has already been calculated, 
## the inverse will be recovered from the cache.



## This function will create a special Matrix 
## object and cache the inverse of this Matrix.
## This function uses a list of functions which are:
## 1. Set the value of the array.
## 2. Get the value of the array.
## 3. Set the value of the inverse.
## 4. Get the value of the Inverse.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y){
            x <<- y
            I <<- NULL
        }
        get <- function(){ x }
        setInverse <- function(Inverse){ I <<- Inverse}
        getInverse <- function(){ I }
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function starts from the special array object created in our previous function.
## First it will confirm if the inverse of our matrix has already been calculated. 
## If this is not the case, it will calculate the inverse of the array and 
## send the value in the cache using the setInverse function.



cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        if(!is.null(I)){
          message("getting cache data")
          return(I)
        }
        Matrix <- x$get()
        I <- solve(Matrix, ...)
        x$setInverse(I)
        I
}

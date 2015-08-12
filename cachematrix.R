
## function which creates a matrix who can cache its inverse . The matrix can be instantiated by calling the function 
# makeCacheMatrix(x) where x is a matrix . To modify or access the contents of the matrix you can use the corresponding
# getter and setter functions get(),set() to manipulate the matrix and getInv(),setinv(x) to manipulate the inverse
makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function()x
    getInv <- function () i
    setInv <- function(inv){
        i <<- inv
    }
    
    list(set=set,get=get,getInv=getInv,setInv=setInv)
    
}


# function who evaluates the inverse of a matrix which was build with the function defined above . The function first checks if the inverse
# value has already been calculated and if so returns the cached value otherwise it computes the inverse and caches it for
# subsequent function calls . The matrix is assumed to be invertible

cacheSolve <- function(x=makeCacheMatrix()) {
    ## Return a matrix that is the inverse of 'x'
    inv <- if(!is.null(x$getInv())){
        x$getInv()
    }else{
        inverse <-solve(x$get())
        x$setInv(inverse)
        inverse
    }
    inv
    
    
    
    
}

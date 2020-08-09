##This function Function makeCacheMatrix can get a matrix as an input and chache the inverse Matrix. 


makeCacheMatrix <- function(x = matrix()) {
         
        invMatrix <- NULL
        setMatrix <- function(y) {
                
                x <<- y
                invMatrix <<- NULL
                
        }
        
    
        getMatrix <- function() x                              #get the value of the Matrix
        setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix
        getInverse <- function() invMatrix                     #get the value of the invertible matrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
 
        
}



## This function computes the inverse of the special "matrix" created by 
#makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then it should retrieve the inverse from the cache




cacheSolve <- function(x, ...) {
        

        invMatrix <- x$getInverse()
        
        if(!is.null(invMatrix)) {                             #if inverse matrix is not NULL
                
                message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
                return(invMatrix)                             #return the invertible matrix
                
        }


        MatrixData <- x$getMatrix()                     #get the original Matrix Data 
        invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
        x$setInverse(invMatrix)                         #set the invertible matrix 
        return(invMatrix)                               #return the invertible matrix
        
        
}




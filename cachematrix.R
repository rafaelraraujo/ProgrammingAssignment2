## Rafael Ramos Araújo - Jully 2015
## makeCacheMatrix is responsible to crate a matrix and set a inverse in cache.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {


        # assigning inverseMatrix as NULL value
        inverseMatrix <- NULL
        # get function to get Matrix
        get <- function() {
                x
        }
        # set function to set a new Matrix
        set <- function(newMatrix) {
                x <<- newMatrix
                inverseMatrix <<- NULL
        }
        # getInverse to get InverseMatrix
        getInverse <- function() {
                inverseMatrix
        }
        # setInverse with Inverse Matrix solution
        setInverse <- function(solvedMatrix) {
                inverseMatrix <<- solvedMatrix
        }
        #return a list of functions
        list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}

## function cacheSolve computes the inverse of the special "matrix" created by 
## function makeCacheMatrix above

cacheSolve <- function(x, ...) {
        # querying and asssigning inverseMatrix cached value
        inverseMatrix <- x$getInverse()
        
        # checking value of cache and using it if not NULL
        if(!is.null(inverseMatrix)) {
                message("getting cached Inverse Matrix data ....")
                return(inverseMatrix)
        }
        
        # gettting matrix and calculating inverse
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        # set value on inverse matrix in cache
        x$setInverse(inverseMatrix)
        
        ## Return a matrix that is the inverse of 'x'
        return(inverseMatrix)
}

## How to validate that:
## Validate one:
# source("cachematrix.R")			#Source the file
# A <- makeCacheMatrix(rbind(c(4,5),c(2,1)))	#set a matrix  
# mat1$get()					#check the Matrix created 
###### Expected result as :
#     [,1] [,2]
#[1,]    4    5
#[2,]    2    1 
# class(A)					#check and confirm the class of A as list
# A$getInverse()   				#check the Inverse initial value is NULL
###### Confirm the inverse value
# cacheSolve(A)
###### Expected result as :
#           [,1]       [,2]
#[1,] -0.1666667  0.8333333
#[2,]  0.3333333 -0.6666667
###### Confirm the inverse value into A
# A$getInverse()
###### Expected result as :
#           [,1]       [,2]
#[1,] -0.1666667  0.8333333
#[2,]  0.3333333 -0.6666667

## Validate two:
# A$set(matrix(15:12,2))   			#set a new matrix using set function
# A$get() 					#check if new matrix was set through set function
# A$getInverse()				#check the Inverse initial value is NULL
# cacheSolve(A)					#get the inverse of matrix and set the cache
###### Expected result as :
#     [,1] [,2]
#[1,]   -6  6.5
#[2,]    7 -7.5    					
# cacheSolve(A)    				#check the inverse of matrix and inverse is being used from cache
# A$getInverse()   				#check the inverse has been set in cache

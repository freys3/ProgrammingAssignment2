## This program contains two main functions "makeCacheMatrix" and "cacheSolve"
## used for the purpose of determining the inverse of an invertible square 
## matrix. The resulting inverse is stored to a cache, so that if the same
## matrix is called, the inverse does not have to be re-computed. Only when
## there is a change to the matrix is a new inverse computed.
##
## Steps: 1.) Assign a variable to the return of the "makeCacheMatrix" using a 
##            matrix as input.
##            e.g., a <- makeCacheMatrix(matrix(rnorm(1:9), 3, 3)             
##        2.) Assign a variable to the return of "cacheSolve" using cached matrix 
##            as input.
##            e.g., b <- solveCache(a)
##        3.) Inverse of the cached matrix is stored in the cache.
##            e.g., type b to display the inverse to the console.
##        4.) If the next matrix called is identical to the previous matrix
##            then the Inverse of the matrix will not be re-computed but
##            retrieved from the cache. 
##            e.g., c <- a
##            e.g., b <- solveCache(c)
##                  ...getting cached data
##        5.) If the next matrix cached is NOT identical to the previous matrix
##            then a new Inverse is computed and stored in the cache.
##            e.g., d <- makeCacheMatrix(matrix(rnorm(1:16), 4, 4)             
##            e.g., b <- solveCache(d)
##            e.g., type b to display the inverse to the console.
##

## "makeCacheMatrix": This function inputs a matrix and creates a special cache 
## matrix that allows the return values of other functions to be stored in a List 
## as a function of the cached matrix. 

makeCacheMatrix <- function(x = matrix()) {                        ##Function that defines functions to be used and stores values returned to a List
        InvMtrx <- NULL                                            ##Initialize Inverse of Matrix equal to Null
        PrvMtrx <<- NULL                                           ##Initialize Previous Matrix equal to Null in the working environment

        setMtrx <- function(y) {                                   ##Function called SetMtrx
                x <<- y                                            ##Stores Matrix passed to function as Matrix X
                InvMtrx <<- NULL                                   ##Sets Inverse to Null in the working environment            
        }
        getMtrx <- function() x                                    ##Anonymous function getMtrx; returns Matrix X
        setInvMtrx <- function(Inverse) InvMtrx <<- Inverse        ##Anonymous function setInvMtrx; sets value of InvMtrx 
        getInvMtrx <- function() InvMtrx                           ##Anonymous function getInvMtrx; returns stored Inverse of Matrix X
        setPrvMtrx <- function(datX) PrvMtrx <<- datX              ##Anonymous function setPrvMtrx; stores Previous Matrix used to PrvMtrx
        getPrvMtrx <- function() PrvMtrx                           ##Anonymous function getPrvMtrx; returns the Previous Matrix used

        list(setMtrx = setMtrx, getMtrx = getMtrx,                 ##Returns a List of the stored values from each of the function returns; list names
           setInvMtrx = setInvMtrx,                                ##same as the returned variable names.
           getInvMtrx = getInvMtrx,
           setPrvMtrx = setPrvMtrx,
           getPrvMtrx = getPrvMtrx)          
}



## "cacheSolve": Returns determines the inverse of matrix 'x' and stores the inverse to the cache.
## If the matrix is identical to the last matrix called and is not Null, then the functions retrieves
## the Inverse stored previously in the cache. Otherwise, the function computes the Inverse 
## of matrix 'x' and stores the computed Inverse to the cache. 
cacheSolve <- function(x, ...) {
        datX <- x$getMtrx()                                       ##Gets the Matrix for computing the Inverse
        PrvMtrx <- x$getPrvMtrx()                                 ##Gets the Previous Matrix
        InvMtrx <- x$getInvMtrx()                                 ##Gets the value of the Inverse of the Matrix currently stored

        if(!is.null(InvMtrx) && identical(datX, PrvMtrx)) {       ##If the Inverse is not Null and Matrix is identical to the Previous then
                message("getting cached data")                    ##Print message to console that cached value will be used
                return(InvMtrx)                                   ##Use Inverse stored in cache
        }
        InvMtrx <- solve(datX)                                    ##If Inverse is Null or the Matrix is not equal to Previous then compute Inverse

        x$setInvMtrx(InvMtrx)                                     ##Store the computed Inverse of the Matrix
        x$setPrvMtrx(datX)                                        ##Set Previous Matrix equal to the Matrix
        InvMtrx                                                   ##Return the Inverse for the Matrix
}

## A pair of functions for calculating and caching the inverse
## of a matrix

######
#Tests at the end of this file
#Inline comments FOLLOW the line of code to which they relate
######

##makeCacheMatrix
#A function intended to work with cacheSolve that stores a matrix
#and caches its inverse (if calculated by cacheSolve)


makeCacheMatrix <- function(m = matrix()){
        #set up the function and args with a default of an empty matrix
        cacheinv <- NULL
        #empty any cache that might already be there
        #
        set <- function(y) {
                m <<- y
                #write new values into m in parent environment
                cacheinv <- NULL
                #clear cache in parent environment
        }
        #set up a way to reset values of an active instance
        #
        get <- function() m
        #set up a way to get the matrix from an active instance
        #       
        setinv <- function(solve) cacheinv <<- solve
        #set up a way to send an inverted matrix into the cache
        #and make it available to the parent environment
        #
        getinv <- function() cacheinv
        #set up a way to get the inverted matrix from an active instance
        # 
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        #make all those functions available to the parent environment
        #with names
}

######

##cacheSolve
#A function to computes the inverse of the special "matrix" returned 
#by makeCacheMatrix. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.

cacheSolve <- function(m, ...){
        #set up the function and args
        cacheinv <- m$getinv()
        #try to get a cached inverted matrix from the instance of
        #makeCacheMatrix that was passed in.
        if(!is.null(cacheinv)){
                #test if there is something in the cache
                message ("fetching cached data")
                #inform user that data is from cache
                return(cacheinv)
                #provide inverted matrix
        }
        data <- m$get()
        #fetch the matrix from the instance of makeCacheMatrix 
        #that was passed in.
        cacheinv <- solve(data, ...)
        #calculate the inverted matrix
        m$setinv(cacheinv)
        #write the inverted matrix to the instance of makeCacheMatrix 
        #that was passed in.
        cacheinv
        #provide inverted matrix
}

#####

##How to exercise these functions

#set up A as a list of 4 functions seeded with an invertable matrix
A <- makeCacheMatrix(matrix(rnorm(1:9), nrow=3, ncol=3))

#view the content of the matrix set above
A$get()

#view the content of the inverted matrix set above. Should be NULL
A$getinv()

#solve and cache a
cacheSolve(A)

#fetch from cache
cacheSolve(A)

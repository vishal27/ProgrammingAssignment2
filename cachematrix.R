## Put comments here that give an overall description of what your
## functions do

## A short comment describing function makeCacheMatrix
## This function basically defines a list of four functions
## These fuctions are used to create a matrix and then calculate it inverse
##Note that input matrix must be a square matrix to be invetible
## Use of "superassignment" operator is particualr interesting, it lets us use these four 
##functions while getting access to its "parent" enviornment or enviorment in which is defined
##particualrly useful to defining family of functions which must have access to common envirment 
## but act as indepedent units, this is termed as "clojure" 
## I have attmepted to comment on when and why "<<-" operator is used

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y    # To make sure right value of x is returned when get() is called
                i <<- NULL 
                
                #this is to make sure that everytime new set of values is assigned
                # "i" is reset to NULL otherwise we will get "getting cached data"
                # even when we are running function with a new set of values for first time
                
        }
        get <- function() x
        setinv <- function(inv) i <<- inv # this linke makes sure cahcing happens
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## ## A short comment describing function cacheSolve
## It uses solve() to find inverese of the matrix
## It relies on makeCacheMatrix for value of "i" which is essential to if matrix 
## data was supplied for first time or was it a request for inverese previously served
## If inverese was already calucated, then its "state" is stored in "i" using "<<-" operator
## otherwise inverse it calculated form scratch.

cacheSolve <- function(x, ...) {
               ## Return a matrix that is the inverse of 'x'
        
                i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

# ## Following shows the batteries of test I used to test above function
# ## Note that all credit goes to "Scott" on coursera forum for genrating these test cases
# 
# 
# create cacheable matrix object
# m<- makeCacheMatrix( )
# 
# # initailize with a an easy to inspect matrix 
# m$set( matrix( c(0, 2, 2, 0 ), 2, 2))
# 
# # note use of parens to retrive the matrix part of the object
# m$get()
# #     [,1] [,2]
# #[1,]    0    2
# #[2,]    2    0
# 
# # test the inverse cacher
# cacheSolve( m )
# #       [,1] [,2]
# # [1,]  0.0  0.5
# # [2,]  0.5  0.0
# 
# # and again... should be cached now
# cacheSolve( m )
# #getting cached data <-- NOTE THE MESSAGE
# # ...
# 
# # test that the inverse works and experiment with how to use the functions
# # m$get() returns the matrix and cacheSolve(m) returns the inverse that we can 
# # use like regular matrices to do things like multilplications...
# #
# # product of matrix mult should be identity matrix AND we should get the cached message
# m$get() %*% cacheSolve(m)
# #getting cached data  <-- Yup... cached!
# #     [,1] [,2]
# #[1,]    1    0
# #[2,]    0    1       <-- eye() think it's and identity Matrix...
# 
# # let R test identify for us
# all.equal( diag(2), m$get() %*% cacheSolve(m) )
# # getting cached data <-- hey.. it's still cached
# #[1] TRUE             <-- R agrees it's an identity
# 
# # save the inverse off and let's see if we can break it...
# m1<- cacheSolve(m)
# 
# # set m to some new values
# m$set( matrix( rnorm(4), 2, 2) )
# 
# # does fetching the inverse without any pre-caching work?
# all.equal( diag(2), m$get() %*% cacheSolve(m) )
# #[1] TRUE             <-- yup, sure does...
# 
# # what about testing m x the inverse we squirreled away in m1?
# all.equal( diag(2), m$get() %*% m1 )
# #[1] "Mean relative difference: xxxx"  <-- Nah... that's a train wreck.  
# 
# # try a bigger matrix and see if we can notice the caching effects
# m$set( matrix( rnorm( 1000000 ), 1000, 1000 ) )
# cacheSolve(m)
# cacheSolve(m)
# # on my quad i5, seems like the second call is faster...
# 
# # and double check it all still works...
# all.equal( diag( 1000 ), m$get() %*% cacheSolve(m) )
# # getting cached data
# # [1] TRUE
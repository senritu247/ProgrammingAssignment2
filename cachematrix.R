## The two functions below creates a special object that stores the matrix 
## and cache's it's inverse

## The first function,makeCacheMatrix creates a matrix which is a list 
## containing a function to 
## 1)set the matrix
## 2)get the matrix
## 3)set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
                   inv<-NULL
                   set<-function(y){
                        x<<-y
                        inv<<-NULL
                      }
                  get<-function()x
                  setinv<-function(solve) inv<<-solve
                  getinv<-function() inv
                  list(set=set,get=get,
                      setinv=setinv,
                       getinv=getinv)
 }


## This function calculates the inverse of the matrix created from above.However
## it first checks that the inverse is already calculated.If so, it gets the 
## inverse from the cache.Otherwise it calculates the inverse of the matrix ands
## sets the inverse in the cache via setinv function

cacheSolve <- function(x, ...) {
                 inv<-x$getinv()
                 if(!is.null(inv)){
                     message("getting cached data")
                     return(inv)
                   }
                 data<-x$get()
                 inv<-solve(data,...)
                 x$setinv(inv)
                 inv
 }


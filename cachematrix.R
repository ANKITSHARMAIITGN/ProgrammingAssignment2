##MATRIX INVERSION IS USUALLY A COSTLY COMPUTATION AND THERE MAY BE SOME BENEFIT TO CACHING THE INVERSE OF A MATRIX
##RATHER THAN COMPUTE IT REPEATEDLY.HERE WE WILL WRITE A PAIR OF FUNCTION THAT CACHE THE INVERSE OF MATRIX

## THIS FUNCTION CREATES A SPECIAL MATRIX OBJECT THAT CAN CACHE IT'S INVERSE

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function()inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
##THIS FUNCTION COMPUTE THE INVERSE OF THE SPECIAL MATRIX RETURNED BY makeCacheMAtrix above.IF THE INVERSE HAS ALREADY
#BEEN CALCULATED (AND THE MATRIX HAS NOT CAHNGED),THEN THE cacheSolve SHOULD SOLVE RETRIEVE THE INVERSE FROM THE CACHE

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinverse(inv)
        inv
}


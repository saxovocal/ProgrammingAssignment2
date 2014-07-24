## The functions evaluates the inverse of a matrix
## but stores it as a cache once evaluated once to 
## prevent re-evaluation. 

## makeCacheMatrix is a list of four functions, as follows:
## 1) to set the matrix
## 2) to get the matrix
## 3) to set the matrix inverse
## 4) to get the matrix inverse
## when input x to function, x must be matrix
## output is list of functions storing input matrix

makeCacheMatrix <- function(x = matrix()) 
{
i<-NULL
set<- function(y)	{
			x<<-y
			i<<-NULL
			}
get<- function()x
setinv<- function(inverse) i<<-inverse
getinv<- function()i
list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve evaluates the matrix from output of
## makeCacheMatrix, or calls already evaluated cache
## from the same function

cacheSolve <- function(x, ...) 
{
i<-x$getinv()
if(!is.null(i))	{
			message("getting cached data")
			return(i)
			}
data<-x$get()
i<-solve(data,...)
x$setinv(i)
i       
}

## Programa referente ao curso de R da Coursera, com finalidade de 
## utilizar a metodologia de escopo

## Armazena em cache matrix e seu inverso, permitindo sua recupera��o 
## sem novo c�lculo

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(n) {
                x <<- n
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
        }


## Calcula o inverso da matriz retornada pela fun��o makeCacheMatrix. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        }

## Como usar
## source("cachematrix.R")
## a <- matrix(1:4,2,2)
## y <- makeCacheMatrix(a)
## cacheSolve(y)

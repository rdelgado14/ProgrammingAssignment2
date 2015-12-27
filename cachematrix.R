makeCacheMatrix <- function(x = matrix()) {

      m_inv <- NULL 

      set <- function(y) {
	       x <<- y
	       m_inv <<- NULL 
      }

      get <- function() x 
      setInv <- function(inv) m_inv <<- inv 
      getInv <- function() m_inv 

      list(set = set, get = get,setInv = setInv,getInv = getInv)
  }


cacheSolve <- function(x, ...) {
      mat <- x$getInv() 

      if(!is.null(mat)) { 
	        return(mat) 
      }

      data <- x$get() 
      mat <- solve(data) 
      x$setInv(mat) 
      mat 
  }


# TEST
# g <- matrix(c(1,32,-1,3,13,19,7,10,-22),3,3)
# h_plus <- makeCacheMatrix(g)
# h_plus_inv <- cacheSolve(h_plus)
# identity <- g %*% h_plus_inv
# identity

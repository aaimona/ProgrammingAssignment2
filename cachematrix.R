## These two functions used together create a convenient way to invert a given matrix,
## assuming that it is always invertible. 

## First function, makeCacheMatrix(), creates a wrapping object 
## that represents the input matrix, cached solution and following functions:
## - set, which sets the matrix for the input,
## - get, which returns the input matrix,
## - ss, which sets the solution (i.e. the solved matrix),
## - gs, which gets the solution.

makeCacheMatrix <- function(mx = matrix()) {
  
  solution <- NULL
  
  set <- function(new_mx) {
        mx <<- new_mx
  }
  
  get <- function() mx
  
  set_solution <- function(new_solution) {
        solution <<- new_solution
  }
  
  get_solution <- function() solution
  
  
  list(set = set, get = get, ss = set_solution, gs = get_solution)
  
}


## This function returns the inverted matrix which is done by:
## (1) checking if a solution already exists, if so - returns it,
## (2) if the solution is not yet computed, the function computes and returns it.

cacheSolve <- function(a) {
  
  ex_sol <- a$gs()
  
  if(!is.null(ex_sol)) {
        return(ex_sol)
    
  }
  
  cal_sol <- solve(a$get())
  a$ss(cal_sol)
  return(cal_sol)
  
}

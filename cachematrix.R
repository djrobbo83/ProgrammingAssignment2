## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                   #INITIALISE OBJECTS, x is initialised in function Argument; Set inv = NULL      
                                #SO ITS AN OJECT IN MakeCacheMatrix environment to be used later in code
  
  set <- function(y) {          #Takes argument named as y, assumed its a matrix       
    x <<- y                     #assigns value on RHS of operatior to an object in parent environment
    inv <<- NULL                #assigns NULL to inv object in parent environment; clearing any value inv which had prev been cached
  }
  
  get <- function() x                         #Gets x from parent environment of makeCacheMatrix
  set_inverse <- function(inverse) inv <<- inverse  # defines setter for inverse uses <<- to assign input arg to inv in parent environment
  get_inverse <- function() inv 
  
  list(set = set,                 #Gives name 'set' to set() function defined above
       get = get,                 #Gives name 'get' to Get() function defined above
       set_inverse = set_inverse, #Gives name 'set_inverse' to set_inverse() function defined above
       get_inverse = get_inverse) #Gives name 'get_inverse' to get_inverse() function defined above
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { #CacheMean required to populate or retrieve mean from makeCacheMatrix
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()         #attempts to retrieve inverse from object passed in as the argument
  if(!is.null(inv)) {            #If isn't null, then retrieves from get_inverse and returns inverse to console
    message("Hold tight...Getting Cached Data!")
    return(inv)
  }
  mat <- x$get() #Gets Matrix from input object
  inv <- solve(mat, ...) #Calculates Inverse
  x$set_inverse(inv)     #Sets inverse in makeCacheMatrix
  inv                    #returns inverse
}

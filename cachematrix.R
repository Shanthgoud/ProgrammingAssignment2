## Set the input 'x' as matrix
## solved value 's' as null

## Every references mean to solve
makeCacheMatrix <- function(x = matrix()) {
s <- null
set <- function(y) <- {
        x<<-y
        s<<-null
}
get <- function() x
        setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

## changed 'mean' to 'solve' and 'm' to 's'

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

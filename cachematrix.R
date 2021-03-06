makeMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
 {
    x <<- y
    m <<- NULL}
  get <- function() x
  setinv <- function(matrix) m <<- matrix
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)}


cacheInv <- function(x) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)}
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m}

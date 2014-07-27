cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()  # uses get vector from makeVector function to 
  m <- mean(data, ...)
  x$setmean(m)
  m
}
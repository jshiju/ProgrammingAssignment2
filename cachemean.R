
# A function which implements a special object to store a numeric vector
# and provide convenience methods to set and get that vector object and the
# and the mean value of the vector
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# A function thats takes a special object create using makeVector() function
# and claculate the mean value and returns either the calculated or cached value
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}



###########################################################
# [USAGE]
# > v <- makeVector(0:1000000)
# > system.time(cachemean(v))
# user  system elapsed 
# 0.08    0.00    0.08 
# > system.time(cachemean(v))
# getting cached data
# user  system elapsed 
# 0       0       0 
# > v <- makeVector(0:1000000)
# > cachemean(v)
# [1] 5e+05
# > cachemean(v)
# getting cached data
# [1] 5e+05
# > 
###########################################################
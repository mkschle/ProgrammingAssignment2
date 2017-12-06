# For compatibility with 2.2.21
.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}


# Path to installed lesson
.lessonpath <- file.path(.get_course_path(), "R_Programming",
                         "lapply_and_sapply")
# Path to dataset
.datapath <- file.path(.lessonpath, "flag.data.txt")
# Load dataset
flags <- read.csv(.datapath, header=FALSE)
# Set column names
colnames(flags) <- c("name", "landmass", "zone", "area", "population",
                     "language", "religion", "bars", "stripes", "colours",
                     "red", "green", "blue", "gold", "white", "black",
                     "orange", "mainhue", "circles", "crosses", "saltires",
                     "quarters", "sunstars", "crescent", "triangle",
                     "icon", "animate", "text", "topleft", "botright")
# Path to dataset info
.infopath <- file.path(.lessonpath, "flag.names.txt")
# Function for user to open info
viewinfo <- function() {
  file.edit(.infopath)
  return(.infopath)
}

# Dummy function to advance user past question for which 
# correct answer yields an error
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Define function to set the value of the matrix. It also clears the old
  # inverse from the cache
  set <- function(y) {
    x <<- y    # Set the value
    m <<- NULL # Clear the cache
  }
  # Define function to get the value of the matrix
  get <- function() x
  # Define function to set the inverse. This is only used by getinverse() when
  # there is no cached inverse
  setInverse <- function(inverse) m <<- inverse
  # Define function to get the inverse
  getInverse <- function() m
  
  # Return a list with the above four functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


  #' Return inverse of matrix x
  #'
  #' This function computes the inverse of the special "matrix" returned by  
  #' makeCacheMatrix above. If the inverse has already been calculated
  #' (and the matrix has not changed), then the cachesolve retrieves the 
  #' inverse from the cache.
  #'
  #' @param x a special matrix created with makeCacheMatrix
  #'
  #' @return The inverse of the matrix x
  #'
  cacheSolve <- function(x) {
    m <- x$getInverse() # This fetches the cached value for the inverse
    if(!is.null(m)) { # If the cache was not empty, we can just return it
      message("getting cached data")
      return(m)
    }
    # The cache was empty. We need to calculate it, cache it, and then return it.
    data <- x$get()  # Get value of matrix
    m <- solve(data) # Calculate inverse
    x$setInverse(m)  # Cache the result
    m                # Return the inverse
  }
  
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
  

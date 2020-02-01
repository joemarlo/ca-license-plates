

fix_NA <- function(x, replacement.value = "-") {
  # replaces NAs with x
  
  x[is.na(x)] <- replacement.value
  x
}


generate_plate <- function(plate.nchar = 7) {
  # function generates a single random string
  #  of 7 letters and numbers 
  
  index <- sample(c(TRUE, FALSE), size = plate.nchar, replace = TRUE)
  plate <- NA
  plate[index] <- LETTERS[sample(1:26, size = plate.nchar, replace = TRUE)][index]
  plate[!index] <- sample(0:9, size = plate.nchar, replace = TRUE)[!index]
  return(paste0(plate, collapse = ""))
}

parse_plate <- function(plate) {
  # function takes a single plate and returns all 
  #  "forward" combinations of the letters
  # e.g. plate "1234" would return "12, 123, 1234, 23, 234, 34"
  
  if (nchar(plate) > 10) stop("Plate must be 10 or less characters)")
  tokens <- c()
  # if plate is just one character
  if (nchar(plate) == 1) {
    tokens <- plate
  } else {
    plate <- strsplit(plate, split = NULL)[[1]]
    len <- length(plate)
    
    # index for storing results
    i <- 1
    # loop through the plate and take every "forward"
    #  combination of letters
    for (bgn in 1:(len - 1)) {
      for (end in (bgn + 1):len) {
        tokens[[i]] <- paste0(plate[bgn:end], collapse = "")
        i <- i + 1
      }
    }
  }
  
  #value controls the maximum characters
  #  is calculated parse_plate('vect with 10 char') %>% length()
  length(tokens) <- 45
  return(tokens)
}
# Test, if HEOM could be made faster
# Trying to vectorize
#
# Just reading from data frame and saving result in a variable increases the time
# to ten-fold
#
# Read data
data <- read.csv("testdata_3.csv")

heom <- function(data) {

  ncols <- ncol(data) - 1
  data <- data[, 1:ncols]
  
  factors <- sapply(data, is.factor)
  numerics <- sapply(data, is.numeric)
  ranges <- apply(data[, numerics], 2, max, na.rm = T) - apply(data[, numerics], 2, min, na.rm = T)

  data <- sapply(data, as.numeric)
  
  dlen <- nrow(data)
  result <- matrix(nrow = nrow(data), ncol = nrow(data))
  row <- vector(mode = "numeric", length = ncol(data))
  
  for (i in 1:dlen) {
    for (j in 1:dlen) {
      
      if(!is.na(result[i, j]))
        next
      
      # For factor attributes, the distance is 1, if they're equal, 0 if not
      row[factors] <- !(data[i, factors] == data[j, factors])
      
      # For numeric type, distance is xi - yi / range
      row[numerics] <- (data[i, numerics] - data[j, numerics]) / ranges
      
      # For NA, distance is 1
      row[is.na(row)] <- 1
      ssum <- sum(row^2)
      result[i, j] <- ssum
      result[j, i] <- ssum
    }
  }
  
  return(result)
}


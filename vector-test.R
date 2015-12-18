# Test, if HEOM could be made faster
# Trying to vectorize
#
# Just reading from data frame and saving result in a variable increases the time
# to ten-fold
#
# Read data
data <- read.csv("testdata_3.csv")

# Number of clumns - the class label column
ncols <- ncol(data) - 1

# Drop class label from data
data <- data[, 1:ncols] # Drop class labels

# Save info about factor columns
factors <- sapply(data, is.factor)
numerics <- sapply(data, is.numeric)
ranges <- apply(data[, numerics], 2, max, na.rm = T) - apply(data[, numerics], 2, min, na.rm = T)
data <- sapply(data, as.numeric)

# Save the length of data (the number of rows)
dlen <- nrow(data)

result <- matrix(nrow = nrow(data), ncol = nrow(data))
row <- vector(mode = "numeric", length = ncol(data))

# Record starting time
start  <- proc.time()
for (i in 1:dlen) {
  for (j in 1:dlen) {
    row[factors] <- !(data[i, factors] == data[j, factors])
    row[numerics] <- (data[i, numerics] - data[j, numerics]) / ranges
    row[is.na(row)] <- 1
    result[i, j] <- sum(row^2)
  }
}

print(proc.time() - start)

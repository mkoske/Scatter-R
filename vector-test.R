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

# Create matrix for final result
fresult <- matrix(nrow = nrow(data), ncol = nrow(data))

# Save the length of data (the number of rows)
dlen <- nrow(data)

# Record starting time
start  <- proc.time()

# Save info about factor columns
factors <- sapply(data, is.factor)

# Convert to matrix
data <- as.matrix(data)

for(i in 1:dlen) {
    if(i %% 10 == 0)
        print("Next ten")
    for(j in 1:dlen) {
        a <- data[i, ]
    }
}

print(proc.time() - start)

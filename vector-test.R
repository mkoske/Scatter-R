# Test, if HEOM could be made faster
# Trying to vectorize

data <- read.csv("testdata_3.csv")
ncols <- ncol(data) - 1
data <- data[, 1:ncols] # Drop class labels
fresult <- matrix(nrow = nrow(data), ncol = nrow(data))
dlen <- nrow(data)
start  <- proc.time()
ranges <- vector(mode = "numeric", length = ncol(data))
for(i in 1:dlen) {
    if(i %% 10 == 0)
        print("Next ten")
    for(j in 1:dlen) {
        a <- data[i, ]
    }
}

print(proc.time() - start)

library(clusterSim)
source('./algorithm/scatter.R')

setosa <- sample(1:50, size = 4)
versicolor <- sample(51:100, size = 4)
virginica <- sample(101:150, size = 4)

raw <- iris[c(setosa, versicolor, virginica), ]

data <- data.Normalization(raw[,1:4], type="n4")
data[, ncol(raw)] <- as.numeric(raw[, ncol(raw)])
dimnames(data)[1] <- list(1:12)

dm <- as.matrix(dist(data[, 1:4]))
diag(dm) <- NA
dimnames(dm) <- list(1:12, 1:12)

print("Data for testing is:")
print(data)
print("Now going through distance matrix:")
for(row in 1:nrow(dm)) {
    minim <- min(dm[row, ], na.rm = TRUE)
    print(sprintf("Minimum on row %s is %s (at column %s)", row, minim, which(dm[row, ] == minim)))
}

print("Traversing:")
labels <- traverse(data, dm)
print("Labels collected were:")
print(labels)

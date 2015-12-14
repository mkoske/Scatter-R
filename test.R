library(clusterSim)
source('./algorithm/scatter.R')

setosa <- sample(1:50, size = 4)
versicolor <- sample(51:100, size = 4)
virginica <- sample(101:150, size = 4)

raw <- iris[c(setosa, versicolor, virginica), ]

data <- data.Normalization(raw[,1:4], type="n4")
data[, ncol(raw)] <- raw[, ncol(raw)]

# distm.txt saved from this dm, like write.matrix(as.matrix(dm), "distm.txt")
dm <- as.matrix(dist(data[, 1:4]))
labels <- traverse(data, dm)

write.matrix(dm, "distance-matrix.txt")
print(labels)

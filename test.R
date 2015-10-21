require(clusterSim)
source("./algorithm/scatter.R")
sum <- 0
rounds <- 30

df <- data.Normalization(iris, type = "n4")

for(i in 1:rounds) {
    sum <- sum + scatter(df)
}

print(sum / rounds)

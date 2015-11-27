library(clusterSim)
source('./algorithm/scatter.R')

raw <- read.csv('testdata_3.csv', sep = ',')

df <- data.Normalization(raw[, 1:4], type="n4")
df$class <- raw[, 5]

time <- system.time(dm <- distance(df, distmethod = "heom"))

# print(time)

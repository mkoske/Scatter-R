library(clusterSim)
source('./algorithm/scatter.R')

raw <- read.csv('testdata_3.csv', sep = ',')

# Time with test data
time <- system.time(dm <- distance(raw, distmethod = "heom", nominals = c(5, 7)))

print(time)

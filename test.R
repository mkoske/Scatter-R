require(clusterSim)
source("./algorithm/scatter.R")

# Testruns for BUBA-data

raw <- read.csv('./data/buba_for_matlab.dat', sep="\t")
buba <- raw[, 3:8]
buba$class <- raw[, 2]

s <- run(buba)
z <- baseline(buba$class)
F <- spower(z, s)

require(clusterSim)
source("./algorithm/scatter.R")

raw <- read.csv('../testdata_3.csv')
tdat <- data.Normalization(raw, type="n4")

tdat$l <- c(1, 2, 2, 3)



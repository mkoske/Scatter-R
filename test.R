require(clusterSim)
source("./algorithm/scatter.R")

files <- c(
    "data/bupa.csv", # 1 = id, 2 = class
    "data/ecoli.csv", # 1 = id, 2 = class
    "data/haberman.csv", # 1 = id, 2 = class
    "data/new-thyroid.csv", # 1 = id, 2 = class
    "data/pima.csv" # 1 = id, 2 = class
)

for(f in files) {

    raw <- read.csv(f, sep = "\t")

    data <- raw[, 3:ncol(raw)]
    data <- data.Normalization(data, type = "n4")
    data$class = raw[, 2]

    print(f)
    print("----------------")
    print("s: ")
    print(run(data))
    print("z")
    base <- baseline(as.vector(data[, 2]), rounds = 50)
    print(base$mean)
}




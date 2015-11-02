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

    print(f)
    raw <- read.csv(f, sep = "\t")

    # 3 to ncol(data) are data columns, 1 and 2 are id and class, respectively
    data <- raw[, 3:ncol(raw)]

    # Normalize data
    data <- data.Normalization(data, type = "n4")

    # Attach class column to normalized data frame
    data$class = raw[, 2]

    s <- run(data, rounds = 15)
    base <- baseline(as.vector(data[, 2]), rounds = 50)

    print(sprintf("| %s | %f | %f ", f, s, base$mean))
}




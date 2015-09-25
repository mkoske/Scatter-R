# ##
# Scatter algorihm implementation
# ##

scatter <- function(df) {

    # Number of columns excluding label, which is the last
    columns <- ncol(df) - 1

    # Calculate distance matrix
    distances <- as.matrix(dist(df[, 1:columns], method = "euclidean"))

}

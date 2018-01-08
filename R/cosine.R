# ##
#' An implementation of cosine.
#' 
#' @param data The Data frame
#' @return Distance matrix
#' @export
# ##
cosine <- function(data) {

    n <- nrow(data)
    p <- ncol(data)
    result <- matrix(nrow = n, ncol = n)

    for (i in 1:n) {
        for (j in 1:n) {

            if (!is.na(result[i, j]))
                next

            a <- as.numeric(data[i,])
            b <- as.numeric(data[j,])

            sim <- 1 - (a %*% b) / (sqrt(a %*% a) * sqrt(b %*% b))
            result[i, j] <- sim
            result[j, i] <- sim
        }

    }
    result
}

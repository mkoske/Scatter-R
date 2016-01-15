# ##
#' An implementation of HEOM metric.
#'
#' This creates distance matrix from given data.
#'
#' TODO: This breaks, if the data has only one column. Also, check if the sapply-
#' way to check numerics and factors is correct, since if there's only one column,
#' it returns logical vector for all cases, not for the column.
#'
#' For further reading, see [1].
#'
#' [1] Wilson, D. Randall, and Tony R. Martinez. 1997. “Improved Heterogeneous
#' Distance Functions.” Journal of Artificial Intelligence Research 6: 1–34.
#' doi:10.1613/jair.346.
#'
#' @param data The data to calculate the distance matrix from
#' @return Returns a distance matrix
#' @export
# ##
heom <- function(data) {

    ncols <- ncol(data) - 1
    data <- data[, 1:ncols]

    factors <- sapply(data, is.factor)
    numerics <- sapply(data, is.numeric)
    ranges <- apply(data[, numerics], 2, max, na.rm = T) - apply(data[, numerics], 2, min, na.rm = T)

    data <- sapply(data, as.numeric)

    dlen <- nrow(data)
    result <- matrix(nrow = nrow(data), ncol = nrow(data))
    row <- vector(mode = "numeric", length = ncol(data))

    for (i in 1:dlen) {
        for (j in 1:dlen) {


            if(!is.na(result[i, j]))
                next
            
            # For factor attributes, the distance is 1, if they're equal, 0 if not
            row[factors] <- !(data[i, factors] == data[j, factors])
            
            # For numeric type, distance is xi - yi / range
            row[numerics] <- (data[i, numerics] - data[j, numerics]) / ranges
            
            # For NA, distance is 1
            row[is.na(row)] <- 1
            ssum <- sum(row^2)
            result[i, j] <- ssum
            result[j, i] <- ssum
        }
    }

    return(sqrt(result))
}

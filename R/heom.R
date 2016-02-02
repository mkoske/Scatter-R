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

    if(!is.data.frame(data))
        data <- as.data.frame(data)

    # Find out which columns are factor type
    factors <- sapply(data, is.factor)

    # Find out which columns are numeric type
    numerics <- sapply(data, is.numeric)

    # Calculate the ranges of numeric variables; it's needed to calculate the distance
    # for numeric variables.
    ranges <- apply(data[numerics], 2, max, na.rm = T) - apply(data[numerics], 2, min, na.rm = T)

    # Convert to numeric whole data
    # TODO: Why?
    data <- sapply(data, as.numeric)

    dlen <- nrow(data) # Distance matrix is an n by n matrix, where n is the number of rows in the data.
    result <- matrix(nrow = nrow(data), ncol = nrow(data))
    row <- vector(mode = "numeric", length = ncol(data))

    # Two loops to calculate the distance between both
    for (i in 1:dlen) {
        for (j in 1:dlen) {

            # This is used to skip about half of the calculations, since the
            # distance between case x and y is same as between y and x.
            if(!is.na(result[i, j]))
                next

            # For factor attributes, the distance is 1, if they're equal, 0 if
            # not. What is inside the parentheses does this: if those factor-
            # columns are same, it results in 1 and if they're not same, the
            # result is zero (0). But since it must be just other way around,
            # we use exclamation mark to invert the result: the distance is
            # zero if they're same and one if they're not same.
            row[factors] <- !(data[i, factors] == data[j, factors])

            # For numeric type, distance is xi - yi / range. The code is just
            # elementwise calculations.
            row[numerics] <- (data[i, numerics] - data[j, numerics]) / ranges

            # For NA, distance is 1
            row[is.na(row)] <- 1

            # Finally square the result and sum it
            ssum <- sum(row^2)
            result[i, j] <- ssum
            result[j, i] <- ssum
        }
    }

    # Since sqrt isn't really fast operation, this is done for all rows at once
    # just before returning the result.
    return(sqrt(result))
}

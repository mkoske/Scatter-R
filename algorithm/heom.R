# ##
# Creates a distance matrix
# ##
heom <- function(data) {

    if(!is.data.frame(data))
        stop("Data must be data frame.")

    print("You chose HEOM as distance method. It's implementation is very slow, so please be patient.")
    calculate <- function(a, b) {

        # a is case a, b is case b, i is the current index, ranges is the value
        # range in in given variable.
        result <- vector(mode = "numeric", length = length(a))
        for(i in 1:length(a)) {

            # Contains missing values
            if(is.na(a[i]) || is.na(b[i])) {
                result[i] <- 1
                next
            }

            if(!is.numeric(a[i])) {
                if(a[i] == b[i])
                    result[i] <- 0
                else
                    result[i] <- 1

                next
            } else {
                # In all other cases, the distance is the ratio of difference and range
                diff <- abs(a[i] - b[i])
                range <- max(data[, i]) - min(data[, i])

                # Temporary hack to prevent dividing with zero
                if(range == 0) {
                    warning("The range was zero; replacing it with 1E-6 to prevent division by zero.")
                    range <- 0.0000001
                }

                result[i] <- (diff / range)
            }
        }

        # sos = sum of squares; note, that distances is a vector and this ^2
        # operation squares every element in the distances vector before summing it.
        sos <- sum(result^2)
        return(sqrt(sos))
    }

    distanceMatrix <- matrix(nrow = nrow(data), ncol = nrow(data))
    for(i in 1:nrow(data)) {
        start <- proc.time()
        for(j in 1:nrow(data)) {
            distanceMatrix[i, j] <- calculate(data[i, ], data[j, ])
        }
        print(proc.time() - start)
    }
}

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

        # 1. NA's
        # 2. Nominals; same or not
        # 3. Numeric

        # In HEOM, if either corresponding value is NA, the distance is 1, so
        # this is, what it does. First, is.na()-calls returns logical vector
        # indicating with TRUE that the value was NA and 0 that it was something
        # else than NA. Next, we use OR operator, to find those that were NA's
        # in either or both. Finally, we convert logical vector to numeric, and
        # the result shows 1's in those places, where either one was NA and zero
        # otherwise.
        nas <- as.numeric(is.na(a) | is.na(b))

        fas <- which(sapply(a, is.factor) == 1)


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

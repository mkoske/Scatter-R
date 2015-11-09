# ## 
# Calculates HEOM-metric between two vecrors
#
# a, b are input vectors
# data is the data frame containing those vectors a and b
# nominal is a vector containing indices or column names that are nominal or 
# ordinal
# ##
heom <- function(a, b, data, nominal = c()) {

    if(!is.vector(a) && !is.vector(b))
        stop("a and b, both, must be vectors")

    # 1. either of values are missing, then distance is 1
    # 2. overlap(xa, ya) if a is nominal or ordinal
    # 3. rn_diff(xa, ya) otherwise

    if(length(a) != length(b))
        stop("Lengths of vectors a and b must be equal")


    veclen = length(a)

    distances <- c() 
    for(i in 1:veclen) {

        if(is.na(a[i]) || is.na(b[i])) {
            distances <- c(distances, 1)
            next
        }

        ## Nominal
        if(any(i %in% nominal)) {

            if(a[i] == b[i])
                distances <- c(distances, 0)
            else
                distances <- c(distances, 1)

            next
        }

        
        diff <- abs(a[i] - b[i])
        range <- max(data[, i]) - min(data[, i])

        distances <- c(distances, diff / range)

    }

    # sos = sum of squares
    sos <- sum(distances^2)

    return(sqrt(sos))
}



# ## 
# Calculates HEOM-metric between two vecrors
#
# a, b are input vectors
# data is the data frame containing those vectors a and b
# nominal is a vector containing indices or column names that are nominal or 
# ordinal
# ##
heom <- function(a, b, data, nominal = c()) {

    if(length(a) != length(b))
        stop("Lengths of vectors a and b must be equal")

    len <- length(a)
    distances <- c()     
    for(i in 1:len) {

        value_of_a <- a[[i]][[1]]
        value_of_b <- b[[i]][[1]]
                
        # Contains missing values
        if(is.na(value_of_a) || is.na(value_of_b)) {
            distances <- c(distances, 1)
            next
        }

        ## Nominal (and ordinal) attributes
        if(any(i %in% nominal) == T) {

            if(value_of_a == value_of_b)
                distances <- c(distances, 0)
            else
                distances <- c(distances, 1)
            
            next
        }

        # In all other cases, the distance is the ratio of difference and range 
        diff <- abs(value_of_a - value_of_b)

        column <- as.vector(data[, i], mode = "double")
        range <- max(column) - min(column)
        
        # Temporary hack to prevent dividing with zero
        if(range == 0) {
            print(sprintf("diff is %s and range is %s", diff, range))
            range <- 0.0000001
        }
        
        distances <- c(distances, diff / range)
    }

    # sos = sum of squares; note, that distances is a vector and this ^2
    # operation squares every element in the distances vector before summing it.
    sos <- sum(distances^2)
    return(sqrt(sos))
}



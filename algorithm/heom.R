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
    result <- mapply(function(a, b, i, data, nominal) {
        
        # Contains missing values
        if(is.na(a) || is.na(b))
            return(1);

        if(any(i %in% nominal) == TRUE) {

            if(a == b)
                return(0)
            else
                return(1)
        }

        # In all other cases, the distance is the ratio of difference and range 
        diff <- abs(a - b)
        column <- as.vector(data[, i], mode = "numeric")
        range <- max(column) - min(column)
        
        # Temporary hack to prevent dividing with zero
        if(range == 0) {
            print("Warning: The range was zero; replacing it with 1E-6 to prevent division by zero.")
            range <- 0.0000001
        }
        
        return(diff / range)   
    }, a, b, i = 1:length(a), nominal, MoreArgs = list(d = data)) 
   
    # sos = sum of squares; note, that distances is a vector and this ^2
    # operation squares every element in the distances vector before summing it.
    sos <- sum(distances^2)
    return(sqrt(sos))
}



# ## 
# Calculates HEOM (Heterogeneous Euclidean Overlap metric) between two vecrors
# `a` and `b`.
# ##
heom <- function(a, b, ranges, nominal = c()) {
   
    if(length(a) != length(b))
        stop("Lengths of vectors a and b must be equal")
    
    # a is case a, b is case b, i is the current index, ranges is the value 
    # range in in given variable.
    result <- mapply(function(a, b, i, ranges, nominal) {
       
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
        range <- max(ranges[, i]) - min(ranges[, i])
       
        # Temporary hack to prevent dividing with zero
        if(range == 0) {
            warning("The range was zero; replacing it with 1E-6 to prevent division by zero.")
            range <- 0.0000001
        }
        
        return(diff / range)   
    }, a, b, i = 1:length(a), MoreArgs = list(ranges = ranges, nominal = nominal)) 
 
    # sos = sum of squares; note, that distances is a vector and this ^2
    # operation squares every element in the distances vector before summing it.
    sos <- sum(result^2)
    return(sqrt(sos))
}



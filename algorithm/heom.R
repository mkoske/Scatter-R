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
    
    result <- mapply(function(a, b, data, nominal) {
                
        # Contains missing values
        if(is.na(a) || is.na(b))
            return 1;

        ## Nominal (and ordinal) attributes
        ## TODO: How to do this in apply?
        ## An idea to above: split the vectors into two, give those that are
        ## nominal, in nominals-vector and rest in the a and b.
        ## Like this: a = (1, 2, 3) and b = (4, 5, 6); nominals_of_a = ("a")
        ## nominals_of_b = ("b"). But this has one problem: they MUST retain the
        ## order, i.e. if there's two nominals, and they're ordered so that 
        ## "x" and "y" in a, and "z" and "w" in b, the ordering must be same so
        ## comparing feature agains feature, like "x" = "z" and "y" = "w", but 
        ## not "x" = "w" or "y" = "z".
        if(any(i %in% nominal) == T) {

            if(a == b)
                distances <- c(distances, 0)
            else
                distances <- c(distances, 1)
            
            next
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
    }, a = a, b = b, MoreArgs = list(data = data, nominal = nominal))
   
    # sos = sum of squares; note, that distances is a vector and this ^2
    # operation squares every element in the distances vector before summing it.
    sos <- sum(distances^2)
    return(sqrt(sos))
}



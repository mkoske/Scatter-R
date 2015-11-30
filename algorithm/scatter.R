# ##
# Scatter algorihm implementation
#
# This is the main function, which groups all smaller parts together and returns
# the final results.
#
# Takes a dataframe as input. Note that the dataframe is assumed to be
# normalized to the range [0, 1].
#
# The classlabel is the name or index of the column containing the classlabel
# and it is moved to the last position and ignored when calculating distances
# etc.
# ##
run <- function(
    data,                           # The data frame to process
    classlabel = NULL,              # Column name or index for classlabel
    distmethod = "euclidean",       # Distance method to use
    usecase = "single",             # Usecase: single, class, variable or all
    iterations = 10,                # Iterations; since random starting point
    baseline_iterations = 50,       # Iterations for statistical baseline
    classes    = c(),               # Which classes are included; others removed
    columns    = c(),               # Which variables are used
    nominals   = c()) {             # Which one are nominal; not in use atm.

    if(!is.data.frame(data))
        stop("Input must be a data frame.")

    # Ensure right class column. If the given class column identifier is
    # the name, then get the index for it. But if it's already numeric,
    # then it probably is the index. Other types are not allowed.
    classIndex <- NULL
    if(class(classlabel) == "character") {
        classIndex <- which(names(data) == classlabel)
    } else if(class(classlabel) == "numeric") {
        classIndex <- classlabel
    } else {
        stop("Invalid class label column given.")
    }
   
    # Move the class label column to last
    classes <- data[, classIndex]
    data[, classIndex] <- NULL
    data$class <- classes
    
    scatters <- vector()
    collectionvector = c()
    lbls <- c()
    
    distance_matrix <- distance(data, distmethod, nominals)

    # Run scatter iterations
    for(i in 1:iterations) {
        print(sprintf("Running iteration %s...", i))

        lbls <- traverse(data, distance_matrix)
        scatters <- c(scatters, scatter(lbls))
        collectionvector <- lbls
    }

    # If there were no labels collected, cannot continue.
    if(length(lbls) < 1) {
        stop("No class labels available, cannot continue.")
    }

    # Calculate statistical baseline.
    baseline <- baseline(lbls, baseline_iterations)

    # Return list of data, that was produced by algorithm.
    return(list(
        iterationvalues = scatters,
        iterationmean = c(sum(scatters) / iterations ),
        sd = sd(scatters),
        collectionvector = collectionvector,
        baseline = baseline
        ))
}

# ##
#
# ##
distance <- function(data, distmethod = "euclidean", nominals = c()) {

    ncols = ncol(data) - 1
    n <- nrow(data)
    
    distances = matrix(nrow = n, ncol = n)

    if(distmethod == "euclidean") {
        distances <- as.matrix(
            dist(data[, 1:ncols], method = "euclidean")
        )
    }
    
    if(distmethod == "manhattan") {
        distances <- as.matrix(
            dist(data[, 1:ncols], method = "manhattan")
        )
    }
    
    if(distmethod == "heom") {
        print("You selected HEOM. This is currently *very* slow. So you must wait.")
        source('./algorithm/heom.R')
        
        classless <- sapply(data[, 1:ncols], as.numeric)
 
        # Get max and min for all columns so they won't be calculated n^2 times
        range <- apply(classless[, 1:ncols], 2, range)

        start <- proc.time()
        distances <- apply(classless, 1, function(row, data, range, nominals) {

            # TODO: It might optimize even more to mark cases done, so it must
            # not go through all n^2 iterations. If the distance between two
            # cases is already calculated, then just return. But how to do this?
            return(apply(data, 1, function(a, b, range, n) {
                return(heom(a, b, range, n))
            }, row, range, nominals))
            
        }, classless, range, nominals)
        print(proc.time() - start)   
    }
    
    if(distmethod == "hvdm") {
        stop("Error: HVDM not implemented yet.")
        for(row1 in data) {
            for(row2 in data) {
            }
        }        
    } 
    
    if(any(is.na(distances)) == T) {
        stop("There were NA's in distance matrix.")
    }
    
    
    return(distances)
}

# ##
# Calculate raw Scatter value
# ##
scatter <- function(lbls) {

    nchanges <- lblchanges(lbls)
    thmax <- maxchanges(lbls)

    sval <- nchanges / thmax
    return(sval)
}

# Traverse the dataset using nearest neighbour method, recording label changes
# as we go.
#
# Returns the vector of labels
# ##
traverse <- function(df, distm, seed = F) {

    # Assume last column is class label column, so ignore it. That's why - 1.
    ncols <- ncol(df) - 1
    nrows <- nrow(df)

    # labels
    lbls <- vector(mode = "character")

    # Add column to track visited; important to do it here, NOT before counting
    # the number of columns (see above).
    df$Visited = F

    # TODO: is there a way to extend dist function to handle other methods too?
    diag(distm) <- NA

    # For testing purposes, set always same seed for RNG
    if(seed == T)
        set.seed(123)

    # Pick randomly a starting point
    currentIdx <- sample(1:nrows, size = 1)

    # This seems to work now; but find out if this could be done just using
    # apply and its friends.
    while(nrow(df[df$Visited == F, ]) > 0) {

        # In the end of this function, the distance matrix is full of NAs, so it
        # generates a warning. This is to prevent the warning.
        if(all(is.na(distm[currentIdx, ]))) {
            lbls <- c(lbls, df[currentIdx, (ncols + 1)])
            break
        }

        # Save the label of the current index
        lbls <- c(lbls, df[currentIdx, (ncols + 1)])

        # Also, set the current index visited
        df[currentIdx, "Visited"] <- T

        minima <- min(distm[currentIdx, ], na.rm = T)
        nearest <- which(distm[currentIdx, ] == minima)

        distm[currentIdx, ] <- NA
        distm[, currentIdx] <- NA

        currentIdx <- nearest[1]
    }

    lbls
}

# ##
# Compute the number of label changes
#
# Returns the number of label changes
# ##
lblchanges <- function(lbls) {

    if(!is.vector(lbls)) {
        stop("Not a vector.")
    }

    changes <- 0

    len <- length(lbls)
    for(idx in 1:len) {
        if((idx < len) && (lbls[idx] != lbls[idx + 1]))
            changes <- changes + 1
    }

    return(changes)

}

# ##
# Calculates the theoretical maximum of changes
#
# TODO: better variable naming; classes is not number of classes, but all class
#       labels from the data, e.g. there might be many instances of "class 1"
# ##
maxchanges <- function(classes) {

    if(!is.vector(classes)) {
        stop("Not a vector.")
    }

    n <- length(classes)

    # Sizes of classes
    sizes <- table(classes)

    # Maxima; this returns only one result, even if there are multiple
    maxima <- max(sizes);

    # This is a special case, where there are multiple maximas; in that case,
    # the theoretical maxima is number of classes minus one.
    maximas <- length(as.vector(which(sizes == maxima)))

    if((maximas == 1) && (maxima > (n - maxima))) {
        return(2 * (n - maxima))
    } else {
        return(n - 1)
    }

}

# ##
# ##
# Computes a statistical baseline running the algorithm multiple times and
# calculating the mean of those runs.
#
# Returns mean scatter value
# ##
baseline <- function(classes, iterations = 30) {

    if(!is.vector(classes)) {
        stop("baseline: lbls should be vector")
    }

    scatters <- list(values = c(), mean = c())
    for(i in 1:iterations) {

        smpl <- sample(classes, size = length(classes))
        scatters$values <- c(scatters$values, scatter(smpl))

    }

    scatters$mean <- sum(scatters$values) / iterations

    return(scatters)
}

# Not really needed?
spower <- function(z, s) {
    return(z - s)
}

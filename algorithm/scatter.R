# ##
# Scatter algorihm implementation
#
# This is the main function, which groups all smaller parts together and returns
# the final results.
#
# Takes a dataframe as input. The dataframe is assumed to be normalized to the
# range [0, 1] and the last column contains the class label and thus ignored
# when calculating distances etc.
# ##
run <- function(data, distmethod = "euclidean", iterations = 10, classes = c(), columns = c()) {

    if(!is.data.frame(data))
        stop("df should be data frame")

    scatters <- vector()

    for(i in 1:iterations) {
        lbls <- traverse(data, distmethod)
        scatters <- c(scatters, scatter(lbls))
    }

    # v = values, s = scatter
    return(list(iterationvalues = scatters, iterationmean = c(sum(scatters) / iterations ), sd = sd(scatters)))
}

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
traverse <- function(df, distmethod = "euclidean", seed = F) {

    # Assume last column is class label column, so ignore it. That's why - 1.
    ncols <- ncol(df) - 1
    nrows <- nrow(df)

    # labels
    lbls <- vector(mode = "character")

    # Add column to track visited; important to do it here, NOT before counting
    # the number of columns (see above).
    df$Visited = F

    # TODO: is there a way to extend dist function to handle other methods too?
    distm <- as.matrix(dist(df[, 1:ncols], method = distmethod))
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

    changes

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

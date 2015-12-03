# ##
# This file contains an implementation of scatter algorithm.
# ##

# ##
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
    data,                       # The data frame to process
    classlabel  = NULL,         # Column name or index for classlabel
    distmethod  = "euclidean",  # Distance method to use
    usecase     = "single",     # Usecase: single, class, variable or all
    iterations  = 10,           # Iterations; since random starting point
    baseline_iterations = 50,   # Iterations for statistical baseline
    classes     = NULL,          # Which classes are included; others removed
    columns     = NULL,          # Which variables are used
    nominals    = NULL) {        # Which variables are nominal; not in use atm.

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

    # Select classes
    if(length(classes) > 0)
        data <- data[data[, classIndex] %in% classes, ]

    class_labels <- data[, classIndex]
    data[, classIndex] <- NULL

    if(length(columns) > 0)
        data <- data[, columns]

    # Append th class label column just before starting the calculation.
    data$class <- class_labels
    scatters <- vector()
    collectionvector = c()
    classes <- c()

    print("The head() of data just before calculating distance matrix:")
    print(head(data))

    distance_matrix <- distance(data, distmethod, nominal)

    for(i in 1:iterations) {

        print(sprintf("Running iteration %s", i))

        classes <- traverse(data, distance_matrix)

        if(usecase == "class") {
            uniq <- unique(classes)
            classwise <- list()
            for(c in uniq) {
                tf <- tf(classes, c)
                classwise[[c]] <- scatter(tf)

                if(length(collectionvector) < 1)
                    collectionvector <- classes
            }


        }

        if(usecase == "single") {
            scatters <- c(scatters, scatter(classes))
            if(length(collectionvector) < 1)
                collectionvector <- classes
        }
    }

    # Calculate statistical baseline.
    baseline <- baseline(classes, baseline_iterations)

    # Return list of data, that was produced by algorithm.
    return(list(
        iterationvalues = scatters,
        iterationmean = sum(scatters) / iterations,
        sd = sd(scatters),
        collectionvector = collectionvector,
        baseline = baseline
        ))
}

# ##
#
# ##
distance <- function(
    data,                       # Data frame
    distmethod = "euclidean",   # Distance measure
    nominals = c()) {           # Which columns are nominal; used for HEOM

    if(!is.data.frame(data))
        stop("Data must be a data frame type.")

    ncols <- ncol(data) - 1
    n <- nrow(data)
    result <- matrix(nrow = n, ncol = n)

    distheom <- function(data, nominal = c()) {

        print("You selected HEOM. This is currently *very* slow.")
        source('./algorithm/heom.R')

        # Convert factors to their numeric values
        classless <- sapply(data[, 1:ncols], as.numeric)

        # Get max and min for all columns so they won't be calculated n^2 times
        range <- apply(classless[, 1:ncols], 2, range)

        return(apply(classless, 1, function(row, data, range, nominals) {
            return(apply(data, 1, function(a, b, range, n) {
                return(heom(a, b, range, n))
            }, row, range, nominals))

        }, classless, range, nominals))
    }

    result <- switch(
        distmethod,
        euclidean = as.matrix(dist(data[, 1:ncols], method = "euclidean")),
        manhattan = as.matrix(dist(data[, 1:ncols], method = "manhattan")),
        heom      = as.matrix(distheom(data[, 1:ncols])),
        c()
        )

    if(length(result) == 0)
        stop("Invalid distmethod. Must be 'euclidean', 'manhattan' or 'heom'.")

    return(result)
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
lblchanges <- function(lbls, cls = NULL) {

    if(!is.vector(lbls, mode = "character"))
        stop("Not a vector.")

    changes <- 0
    n <- length(lbls)
    for(idx in 1:n) {
        if((idx < n) && (lbls[idx] != lbls[idx + 1]))
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

    if(!is.vector(classes, mode = "character")) {
        stop("baseline: lbls should be vector")
    }

    scatters <- list(values = c(), mean = c())
    for(i in 1:iterations) {
        smpl <- sample(classes, size = length(classes))
        scatters$values <- c(scatters$values, scatter(smpl))

    }

    scatters$mean <- (sum(scatters$values) / iterations)
    scatters$sd <- sd(scatters$values)

    return(scatters)
}

# Not really needed?
spower <- function(z, s) {
    return(z - s)
}

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
    data,                           # The data frame to process
    classlabel  = NULL,             # Column name or index for classlabel
    distanceMethod  = "euclidean",  # Distance method to use
    usecase     = "single",         # Usecase: single, class, variable or all
    iterations  = 10,               # Iterations; since random starting point
    baselineIterations = 50,        # Iterations for statistical baseline
    classes     = NULL,             # Which classes are included; others removed
    columns     = NULL,             # Which variables are used
    nominals    = NULL) {           # Which variables are nominal; not in use

    if(!is.data.frame(data))
        stop("Input data must be a data frame.")

    if(length(classes) > 0)
        data <- data[data[, classlabel] %in% classes, ]

    class_labels <- data[, classlabel]
    data[, classlabel] <- NULL

    if(length(columns) > 0)
        data <- data[, columns]

    # Move classlabel column to last.
    data$class <- class_labels


    scatter <- switch(usecase,
        single = usecase.single(data, distanceMethod, iterations, nominal),
        class  = usecase.class(data, distanceMethod, iterations, nominal),
        param  = usecase.param(),
        all    = usecse.all())

    baseline <- baseline(data$class, baselineIterations)
    return(list(scatter = scatter, baseline = baseline))
}

usecase.single <- function(data, distanceMethod = "euclidean", iterations = 10, nominal = c()) {

    collectionVector <- vector(length = nrow(data))
    distanceMatrix <- distance(data, distanceMethod, nominal)
    values <- vector(length = iterations)

    for(i in 1:iterations) {
        collectionVector <- traverse(data, distanceMatrix)
        values[i] <- scatter(collectionVector)
    }

    return(list(
        values = values,
        mean = (sum(values) / iterations),
        collectionVector = collectionVector))
}

# ##
# Transforms the class label list so, that it contains only the current label
# and others are counterclass.
recode <- function(labels, current) {
    labels <- as.vector(labels, numeric = "character")
    current <- as.character(current)
    labels[labels != current] <- 0
    return(labels)
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
scatter <- function(labels, current = NULL) {
    changes <- numChanges(labels)
    thmax <- maxChanges(labels, current)
    return(changes / thmax)
}

# ##
# If current is NULL, then consider the largest as current and others counter-
# class; if current is set, then consider current, well, current and others as
# counterclass together. This is like two-class situation.
# ##
maxChanges <- function(labels, current = NULL) {

    nmax <- NULL
    max <- NULL
    n <- length(labels)
    sizes <- table(labels)

    if(is.null(current)) {
        max <- max(sizes);
        nmax <- length(as.vector(which(sizes == max)))
    } else {
        max <- sizes[[current]]
    }

    if((nmax == 1) && (max > (n - max)))
        thmax <- (2 * (n - max))
    else
        thmax <- (n - 1)

    return(thmax)
}

numChanges <- function(labels) {

    n <- length(labels)
    changes <- 0
    for(i in 1:n) {
        if((i < n) && (labels[i] != labels[i + 1]))
            changes <- changes + 1
    }
    return(changes)
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

    return(lbls)
}

# ##
# ##
# Computes a statistical baseline running the algorithm multiple times and
# calculating the mean of those runs.
#
# Returns mean scatter value
# ##
baseline <- function(labels, iterations = 50) {

    n <- length(labels)
    scatters <- vector(length = iterations)
    sapply(1:iterations, function(iteration, labels, n) {
        sample <- sample(labels, size = n)
        scatters[iteration] <- scatter(sample)
    }, labels, n)

    return(sum(scatters) / iterations)
}

# Not really needed?
spower <- function(z, s) {
    return(z - s)
}

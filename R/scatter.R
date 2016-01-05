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

    result <- NULL
    if(usecase == "single") {
        result <- usecase.single(data, distanceMethod, iterations, nominal, baselineIterations)
    } else if(usecase == "classes") {
        distanceMatrix = distance(data, distanceMethod, nominal)
        result <- usecase.class(data, distanceMatrix, iterations, nominal, baselineIterations)
    } else if(usecase == "variables") {
        result  <- usecase.variable(data, distanceMethod, iterations, nominal, baselineIterations)
    } else if(usecase == "all") {
        result <- usecase.all(data, distanceMethod, iterations, nominal, baselineIterations)
    } else {
        stop("Unknown usecase. Must be one of following: single, classes, variables or all.")
    }

    return(result)
}

# ##
# Usecase `variable`. This runs scatter algorithm for each variable separately.
# ##
usecase.variable <- function(data, distanceMethod = "euclidean", iterations = 10, nominal = c(), baselineIterations = 50) {

    # -1 for class column; it's the last one
    variables <- ncol(data) - 1
    
    # Result matrix; rows are variables and columns are iterations, i.e. 
    # `result[i, j]` contains scatter value for variable `i` on iteration `j`.
    result <- matrix(nrow = variables, ncol = iterations)
    
    baselines <- vector(mode = "numeric")
    collectionVector <- vector(mode = "numeric", length = nrow(data))

    for(variable in 1:variables) {
        distanceMatrix <- distance(as.data.frame(data[, variable]), distanceMethod, nominal)
        for(i in 1:iterations) {
            print(sprintf("Running iteration %s for variable %s...", i, variable))
            collectionVector <- traverse(data, distanceMatrix)
            result[variable, i] <- scatter(collectionVector)
        }

        baselines <- c(baselines, baseline(data$class, baselineIterations))
    }

    means <- apply(result, 1, mean)

    return(list(
        values    = result,
        means     = means,
        baselines = baselines
        ))
}

# ##
# Usecase `class`. This runs scatter algorithm for each class separately.
# ##
usecase.class <- function(data, distanceMatrix, iterations = 10, nominal = c(), baselineIterations = 50) {

    classes <- as.numeric(unique(data[, ncol(data)]))
    
    # TODO: Any other ideas to handle this? This is due to fact that R starts it's indexing from 1 instead of
    # zero and class is used also as an index in result matrix.
    if(any(classes == 0))
        classes <- classes + 1
        
    ncols <- ncol(data) - 1

    result <- matrix(nrow = length(classes), ncol = iterations)
    baselines <- vector(mode = "numeric")

    for(class in classes) {
    
        for(i in 1:iterations) {
            print(sprintf("Running iteration %s for class %s...", i, class))
            collectionVector <- as.numeric(traverse(data, distanceMatrix))
            collectionVector[collectionVector != class] <- (-1)
            result[class, i] <- scatter(collectionVector)
        }

        labels <- as.numeric(data$class)
        labels[labels != class] <- (-1)
        baselines <- c(baselines, baseline(labels, baselineIterations))
    }

    means <- apply(result, 1, mean)

    return(list(
        values    = result,
        means     = means,
        baselines = baselines
        ))
}

# ##
# Usecase `single`. This runs scatter algorithm for the whole dataset.
# ##
usecase.single <- function(data, distanceMethod = "euclidean", iterations = 10, nominal = c(), baselineIterations = 50) {

    ncols <- ncol(data) - 1
    collectionVector <- vector(length = nrow(data))
    distanceMatrix <- distance(data[, 1:ncols], distanceMethod, nominal)
    values <- vector(length = iterations)

    for(i in 1:iterations) {
        print(sprintf("Running iteration %s...", i))
        collectionVector <- traverse(data, distanceMatrix)
        values[i] <- scatter(collectionVector)
    }

    baseline <- baseline(collectionVector, baselineIterations)

    return(list(
        values      = values,
        means       = (sum(values) / iterations),
        baselines   = baseline,
        collectionVector = collectionVector
        ))
}

# ##
# Usecase `all`. This function is a bit more complicated than others. It runs the classwise
# analysis for all variables separately, i.e. it runs the usecase `class` for each variable.
# ##
usecase.all <- function(data, distanceMethod = "euclidean", iterations = 10, nominal = c(), baselineIterations = 50) {

    variables <- ncol(data) - 1
    all <- list()
    result <- matrix(nrow = variables, ncol = iterations)
    baselines <- vector(mode = "numeric", length = baselineIterations)
    collectionVector <- vector(mode = "numeric", length = nrow(data))

    for(variable in 1:variables) {

        distanceMatrix <- distance(as.data.frame(data[, variable]), distanceMethod, nominal)

        for(i in 1:iterations) {
            print(sprintf("Running iteration %s for variable %s...", i, variable))
            result <- usecase.class(data, distanceMatrix, iterations, nominal, baselineIterations)
        }

    }

    return(all)
}

# ##
# Distance function.
# 
# Expects dataframe *without* class label column
# 
# For euclidean and manhattan distances, it uses the `dist` function from base and for HEOM,
# it uses the code found in `heom.R`-file.
# ##
distance <- function(
    data,                           # Data frame
    distmethod  = "euclidean",      # Distance measure
    nominals    = NULL) {           # Which columns are nominal; used for HEOM

    if(!is.data.frame(data))
        stop("Data must be a data frame type.")

    if(distmethod == "heom")
        source('algorithm/heom.R')

    result <- switch(
        distmethod,
        euclidean = as.matrix(dist(data, method = "euclidean")),
        manhattan = as.matrix(dist(data, method = "manhattan")),
        heom      = as.matrix(heom(data)),
        c()
        )

    if(is.null(result))
        stop("Invalid distmethod. Must be 'euclidean', 'manhattan' or 'heom'.")

    return(result)
}

# ##
# Calculate raw Scatter value
#
# TODO: Is `current` needed any more?
# ##
scatter <- function(labels, current = NULL) {
    changes <- numChanges(labels)
    thmax <- maxChanges(labels, current)
    return(changes / thmax)
}

# ##
# Calculate theoretical maximum value for label changes.
#
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

    if((nmax == 1) && (max > (n - max))) {
        thmax <- (2 * (n - max))
    } else {
        thmax <- (n - 1)
    }

    return(thmax)
}

# ## 
# Calculate the number of label changes.
#
# If current and next label are not the same, then counter is incremented by one.
# ##
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
traverse <- function(df, distm) {

    # Assume last column is class label column, so ignore it. That's why - 1.
    ncols <- ncol(df) - 1
    nrows <- nrow(df)

    lbls <- vector(mode = "character")

    # Add column to track visited; important to do it here, NOT before counting
    # the number of columns (see above).
    df$Visited = F
    
    # Set the diagonal to NA to ignore it when finding nearest neighbour. Diagonal
    # means the distance between the case itself.
    diag(distm) <- NA

    # Random starting point
    currentIdx <- sample(1:nrows, size = 1)

    # TODO: This seems to work now; but find out if this could be done just using
    # apply and its friends.
    while(nrow(df[df$Visited == F, ]) > 0) {

        # No more cases to visit, so stop here.
        if(all(is.na(distm))) {
            lbls <- c(lbls, df[currentIdx, (ncols + 1)])
            break
        }

        # Save the label of the current index
        lbls <- c(lbls, df[currentIdx, (ncols + 1)])

        # Also, set the current index visited
        df[currentIdx, "Visited"] <- T

        minima <- min(distm[currentIdx, ], na.rm = T)
        minimas <- which(distm[currentIdx, ] == minima)

        distm[currentIdx, ] <- NA
        distm[, currentIdx] <- NA

        if(length(minimas) > 1) {
            nearest <- sample(minimas, size = 1)
        } else {
            nearest <- minimas[1]
        }

        currentIdx <- nearest
    }

    return(lbls)
}

# ##
# Computes a statistical baseline running the algorithm multiple times and
# calculating the mean of those runs.
#
# Returns mean of iteration
# ##
baseline <- function(labels, iterations = 50) {

    print(sprintf("Calculating baseline with %s iterations...", iterations))
    n <- length(labels)
    values <- vector(mode = "numeric", length = iterations)
    for(i in 1:iterations) {
        sample <- sample(labels, size = n)
        values[i] <- scatter(sample)
    }

    return(sum(values) / iterations)
}

# Not really needed?
spower <- function(z, s) {
    return(z - s)
}

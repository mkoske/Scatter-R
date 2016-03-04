# ##
#' Function \code{run} is main entry point to Scatter-algorithm.
#'
#' Function \code{run} takes a dataframe as input, which is only required
#' parameter, and calculates various values from it.
#'
#' \code{run} first performs some checks on parameters, moves the class label
#' column to last position. Then proper \code{usecase} is selected and run.
#' Finally, the result is returned.
#'
#' Usecases perform different calculations: usecase \code{single} runs the
#' scatter algorithm on whole dataset, usecase \code{classes} runs the scatter
#' algorithm on every class in the dataset, usecase \code{variables} runs the
#' scatter algorithm on every variable and finally, usecase \code{all} runs
#' \code{classes} usecase for every variable.
#'
#' @param data A data frame to process
#' @param classlabel Column name or index for class label; if none provided,
#'        last column is assumed
#' @param distanceMethod Distance method to use; must be one of following:
#'        \code{euclidean}, \code{manhattan} or \code{heom}.
#' @param usecase A usecase to select; must be one of following: \code{single},
#'        \code{classes}, \code{variables} or \code{all}
#' @param iterations A number of iterations for Scatter-algorithm
#' @param baselineIterations A number of iteration for baseline calculation
#' @param classes List of class included in the calculation
#' @param columns List of indices or names of columns included in the
#'        calculation
#' @param nominals List of indices or names of those columns that are nominal;
#'        not in use at the moment
#' @param quiet A flag that controls whether messages are printed or not
#' @return This returns whatever \code{usecase.*}-functions return, since this
#'         function is only a wrapper to usecases.
#' @export
#' @examples
#' run(iris)
#' run(iris, classlabel = 5, usecase = "single")
#' run(iris, classlabel = 5, distanceMethod = "manhattan", usecase = "classes", iterations = 30)
# ##
run <- function(
    data,
    classlabel = NULL,
    distanceMethod  = "euclidean",
    usecase     = "single",
    iterations  = 10,
    baselineIterations = 50,
    classes     = NULL,
    columns     = NULL,
    nominals    = NULL,
    quiet       = FALSE) {

    if(!is.data.frame(data))
        stop("Input data must be a data frame.")

    # Include only selected classes
    if(length(classes) > 0)
        data <- data[data[, classlabel] %in% classes, , drop = FALSE]

    # If no classlabel was provided, assume it's the last column
    if(is.null(classlabel))
        classlabel <- ncol(data)

    # Ensure numeric classlabel
    if(!is.numeric(classlabel)) {
        classlabel <- which(names(data) == classlabel)
    }

    # Save class labels to append those at the end of data frame later
    class_labels <- data[, classlabel]
    data[, classlabel] <- NULL

    if(length(columns) > 0)
        data <- data[columns]

    # Move classlabel column to last.
    data[, (ncol(data) + 1)] <- class_labels

    result <- NULL

    # Select proper usecase. Exact case-sensitive match is used here.
    if(usecase == "single") {
        result <- usecase.single(data, distanceMethod, iterations, nominals, baselineIterations, quiet)
    } else if(usecase == "classes") {
        # TODO: Any way to ensure classlabel is correct subscript?
        ncols <- ncol(data) - 1
        distanceMatrix = distance(data[1:ncols], distanceMethod, nominals)
        result <- usecase.class(data, distanceMatrix, iterations, nominals, baselineIterations, quiet)
    } else if(usecase == "variables") {
        result <- usecase.variable(data, distanceMethod, iterations, nominals, baselineIterations, quiet)
    } else if(usecase == "all") {
        result <- usecase.all(data, distanceMethod, iterations, nominals, baselineIterations, quiet)
    } else {
        stop("Unknown usecase. Must be one of following: single, classes, variables or all.")
    }

    return(result)
}

# ##
#' Usecase \code{variable} runs the algorithm for each variable separately.
#'
#' @param data Data
#' @param distanceMethod Distance method
#' @param iterations Number of iterations
#' @param nominal Nominal attributes
#' @param baselineIterations Number of baseline iterations
#' @param quiet A flag that controls whether messages are printed or not
#' @return TBD
#' @examples
#' #TBD
# ##
usecase.variable <- function(
    data,
    distanceMethod = "euclidean",
    iterations = 10,
    nominal = c(),
    baselineIterations = 50,
    quiet = FALSE) {

    # -1 for class column; it's the last one
    variables <- ncol(data) - 1

    # Result matrix; rows are variables and columns are iterations, i.e.
    # `result[i, j]` contains scatter value for variable `i` on iteration `j`.
    result <- matrix(nrow = variables, ncol = iterations)

    baselines <- vector(mode = "numeric")
    collectionVector <- vector(mode = "numeric", length = nrow(data))

    # Iterate over all variables
    for(variable in 1:variables) {

        # In variables usecase, only one variable is used at the time to calculate proximity matrix
        distanceMatrix <- distance(data[variable], distanceMethod, nominal)
        for(i in 1:iterations) {
            if(quiet == FALSE) {
                print(sprintf("Running iteration %s for variable %s...", i, variable))
            }
            collectionVector <- traverse(data, distanceMatrix)
            result[variable, i] <- scatter(collectionVector)
        }

        baselines <- c(baselines, baseline(data[, (variables + 1)], baselineIterations))
    }

    means <- apply(result, 1, mean)

    rowlabels <- names(data)
    rownames(result) <- rowlabels[1:nrow(result)]

    return(list(
        values    = result,
        means     = means,
        baselines = baselines
        ))
}

# ##
#' Usecase \code{class} runs the algorithm for each class separately.
#'
#' @param data Data
#' @param distanceMatrix A distance matrix. This is different than other
#'        usecases as it doesn't take \code{distanceMethod} but
#'        \code{distanceMatrix}. This is because the usecase.class is reused
#'        in usecase.all.
#' @param iterations Number of iterations
#' @param nominal Nominal attributes
#' @param baselineIterations Number of baseline iterations
#' @param quiet A flag that controls whether messages are printed or not
#' @return TBD
#' @examples
#' #TBD
# ##
usecase.class <- function(
    data,
    distanceMatrix,
    iterations          = 10,
    nominal             = c(),
    baselineIterations  = 50,
    quiet = FALSE) {

    # Pick up all unique classes that the data contains
    classes <- as.numeric(unique(data[, ncol(data)]))
    if(any(classes < 0)) {
        stop("Class labels cannot be negative.")
    }

    ncols <- ncol(data) - 1
    # TODO: Don't grow in a loop :)
    result <- vector(mode = "numeric")
    baselines <- vector(mode = "numeric")

    # TODO: does this for loop maintain it's order?
    for(class in classes) {

        for(i in 1:iterations) {

            if(quiet == FALSE) {
                print(sprintf("Running iteration %s for class %s...", i, class))
            }

            collectionVector <- as.numeric(traverse(data, distanceMatrix))
            collectionVector[collectionVector != class] <- (-1)
            result <- c(result, scatter(collectionVector))
        }

         labels <- as.numeric(data[, (ncols + 1)])
         labels[labels != class] <- (-1)
         baselines <- c(baselines, baseline(labels, baselineIterations))
    }

    # The result is stored in the vector above to avoid indexing issues. Then, at last
    # it's converted to matrix form as in other usecases
    result <- matrix(result, ncol = iterations, byrow = TRUE)
    means <- apply(result, 1, mean)

    # This is to add row labels to result matrix. It's easier to see which class got which
    # numbers when there's labels attached. Though the scatter algorithm doesn't change any
    # order of data, at least to my knowledge.
    rowlabels <- unique(data[, ncol(data)])
    rownames(result) <- rowlabels[1:nrow(result)]

    return(list(
        values    = result,
        means     = means,
        baselines = baselines
        ))
}

# ##
#' Usecase `single`. This runs scatter algorithm for the whole dataset.
#'
#' @param data Data
#' @param distanceMethod Distance method
#' @param iterations Number of iterations
#' @param nominal Nominal attributes
#' @param baselineIterations Number of baseline iterations
#' @param quiet A flag that controls whether messages are printed or not
#' @return TBD
#' @examples
#' #TBD
# ##
usecase.single <- function(
    data,
    distanceMethod = "euclidean",
    iterations = 10,
    nominal = c(),
    baselineIterations = 50,
    quiet = FALSE) {

    ncols <- ncol(data) - 1
    collectionVector <- vector(length = nrow(data))
    distanceMatrix <- distance(data[1:ncols], distanceMethod, nominal)
    values <- vector(length = iterations)

    for(i in 1:iterations) {
        if(quiet == FALSE) {
            print(sprintf("Running iteration %s...", i))
        }
        # This usecase returns also a collection vector. It's always the last one
        # since it gets overwritten on every iteration.
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
#' Usecase `all` runs the \code{classes} for all variables separately
#'
#' @param data Data
#' @param distanceMethod Distance method
#' @param iterations Number of iterations
#' @param nominal Nominal attributes
#' @param baselineIterations Number of baseline iterations
#' @param quiet A flag that controls whether messages are printed or not
#' @return TBD
# ##
usecase.all <- function(
    data,
    distanceMethod = "euclidean",
    iterations = 10,
    nominal = c(),
    baselineIterations = 50,
    quiet = FALSE) {

    # This is the container for all results
    all <- list()
    variables <- ncol(data) - 1
    result <- matrix(nrow = variables, ncol = iterations)
    varnames <- names(data)

    # Run classwise analysis for each variable, i.e. loop over all variables and run
    # usecase classes for each.
    for(variable in 1:variables) {
        if(quiet == FALSE) {
            print(sprintf("Running usecase.class for variable %s", variable))
        }

        # New distance matrix for each variable
        distanceMatrix <- distance(data[variable], distanceMethod, nominal)

        # Using quiet = TRUE here to reduce output. If it's FALSE, then it will produce
        # variables * iterations (e.g. 8 variables * 10 iterations = 80lines) lines of output messages.
        # TODO: Think about adding additional flag to control the quietness of this usecase.all and
        # usecase.class separately. Would someone need it?
        result <- usecase.class(data, distanceMatrix, iterations, nominal, baselineIterations, TRUE)
        all[[varnames[variable]]] <- result
    }

    return(all)
}

# ##
#' Distance function wrapper for selecting between euclidean, manhattan
#' and HEOM.
#'
#' For euclidean and manhattan distances, it uses the `dist` function from
#' base and for HEOM, it uses the code found in `heom.R`-file. Note, that
#' this function expects data to be without class-column!
#'
#' @param data The data
#' @param distanceMethod Distance method; must be one of following:
#'        \code{euclidean}, \code{manhattan} or \code{heom}
#' @param nominals Nominal attributes; not in use at the moment
#' @return Returns a distance matrix.
# ##
distance <- function(
    data,                               # Data frame
    distanceMethod  = "euclidean",      # Distance measure
    nominals        = NULL) {           # Which columns are nominal; not in use at the moment

    if(!is.data.frame(data)) {
        stop("Data must be a data frame type.")
    }

    result <- switch(
        distanceMethod,
        euclidean = as.matrix(dist(data, method = "euclidean")),
        manhattan = as.matrix(dist(data, method = "manhattan")),
        heom      = as.matrix(heom(data)),
        c()
        )

    if(is.null(result))
        stop("Invalid distanceMethod. Must be 'euclidean', 'manhattan' or 'heom'.")

    return(result)
}

# ##
#' Calculate raw Scatter value
#'
#' @param labels The set of labels found from data
#' @return Returns a raw Scatter value
# ##
scatter <- function(labels) {

    # Calculate the number of actual changes
    changes <- numChanges(labels)

    # Calculate the theoretical maximum number of changes
    thmax <- maxChanges(labels)

    # Scatter value is the proportion of previous
    return(changes / thmax)
}

# ##
#' Calculate theoretical maximum value for label changes.
#'
#' If current is NULL, then consider the largest as current and others counter-
#' class; if current is set, then consider current, well, current and others as
#' counterclass together. This is like two-class situation.
#'
#' @param labels A set of labels found in dataset that is being processed.
#' @return Returns maximum number of label changes within given set of labels.
# ##
maxChanges <- function(labels) {

    nmax <- 0
    max <- NULL
    n <- length(labels)
    sizes <- table(labels)

    max <- max(sizes);
    nmax <- length(as.vector(which(sizes == max)))

    # Choose the theoretical maximum. See References and further reading from
    # README.md to learn about theory of this.
    if((nmax == 1) && (max > (n - max))) {
        thmax <- (2 * (n - max))
    } else {
        thmax <- (n - 1)
    }

    return(thmax)
}

# ##
#' Calculate the number of label changes
#'
#' If current and next label are not the same, then counter is incremented by
#' one.
#'
#' @param collectionVector A collection vector produced by \code{traverse}.
#' @return The number of changes in the collection vector
# ##
numChanges <- function(collectionVector) {
    n <- length(collectionVector)
    changes <- 0
    for(i in 1:n) {
        if((i < n) && (collectionVector[i] != collectionVector[i + 1])) {
            changes <- changes + 1
        }
    }
    return(changes)
}

# ##
#' Generate collection vector
#'
#' Traverse the dataset using nearest neighbour method, recording label changes
#' as we go.
#'
#' @param data The data
#' @param distanceMatrix The distance matrix calculated from \code{data}
#' @return Returns the vector of labels
# ##
traverse <- function(data, dm, seed=FALSE) {

    # Assume last column is class label column, so ignore it. That's why - 1.
    p <- ncol(data)
    n <- nrow(data)

    indices <- vector(mode = "numeric", length=n)

    if(seed == TRUE)
        set.seed(1)

    current <- sample(1:n, size = 1)
    labels <- data[, p]
    count <- 0


        # Loop until all cases are visited
    while(count <= n) {

        count <- count + 1
        if(count >= n) {
            indices[count] <- labels[current]
            break
        }

        # Save the label of the current index
        indices[count] <- current

        row <- dm[current, ]
        minima <- min(row[-c(indices)])
        minimas <- which(row == minima)

        if(length(minimas) > 1) {

            if(seed == TRUE)
                set.seed(1)

            nearest <- sample(minimas, size = 1)
            nearest <- nearest[[1]]
        } else {
            nearest <- minimas[[1]]
        }

        # The nearest case is the next current case
        current <- nearest
    }

    labels[order(indices)]
}

# ##
#' Computes the statistical baseline
#'
#' Computes a statistical baseline running the algorithm multiple times and
#' calculating the mean of those runs.
#'
#' @param labels Vector of all labels contained in the dataset
#' @param iterations Number of baseline iterations
#' @return Returns mean of iteration
# ##
baseline <- function(labels, iterations = 50) {

    print(sprintf("Calculating baseline with %s iterations...", iterations))
    n <- length(labels)
    values <- vector(mode = "numeric", length = iterations)
    for(i in 1:iterations) {

        # Baseline is a scatter value of random situation. So this is to shuffle
        # or generate random list of classlabels and calculate scatter for it.
        sample <- sample(labels, size = n)
        values[i] <- scatter(sample)
    }

    return(sum(values) / iterations)
}

# ##
#' Calculates separation power
#'
# TODO: Is this function really needed?
#'
#' @param z Baseline
#' @param s Raw Scatter value
#' @return Returns the difference between baseline and raw scatter value.
# ##
spower <- function(z, s) {
    return(z - s)
}

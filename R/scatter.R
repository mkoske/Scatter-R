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
#' @param distance_method Distance method to use; must be one of following:
#'        \code{euclidean}, \code{manhattan} or \code{heom}.
#' @param usecase A usecase to select; must be one of following: \code{single},
#'        \code{classes}, \code{variables} or \code{all}
#' @param iterations A number of iterations for Scatter-algorithm
#' @param baseline_iterations A number of iteration for baseline calculation
#' @param classes List of class included in the calculation
#' @param columns List of indices or names of columns included in the
#'        calculation
#' @return This returns whatever \code{usecase.*}-functions return, since this
#'         function is only a wrapper to usecases.
#' @export
#' @examples
#' run(iris)
#' run(iris, classlabel = 5, usecase = "single")
#' run(iris, classlabel = 5, distance_method = "manhattan", usecase = "classes")
# ##
run <- function(
    data,
    classlabel          = NULL,
    distance_method     = "euclidean",
    usecase             = "single",
    iterations          = 10,
    baseline_iterations = 50,
    classes             = NULL,
    columns             = NULL
) {

    if (!is.data.frame(data))
        stop("Input data must be a data frame.")

    # Include only selected classes
    if (length(classes) > 0)
        data <- data[data[, classlabel] %in% classes, , drop = FALSE]

    # If no classlabel was provided, assume it's the last column
    if (is.null(classlabel))
        classlabel <- ncol(data)

    # Ensure numeric classlabel
    if (!is.numeric(classlabel)) {
        classlabel <- which(names(data) == classlabel)
    }

    class_labels <- data[, classlabel]
    data[, classlabel] <- NULL

    if (length(columns) > 0)
        data <- data[columns]

    data[, (ncol(data) + 1)] <- class_labels

    result <- NULL
    if (usecase == "single") {
        result <- usecase.single(
            data,
            distance_method,
            iterations,
            baseline_iterations
        )
    } else if (usecase == "classes") {
        ncols <- ncol(data) - 1
        distance_matrix <- distance(data[1:ncols], distance_method)
        result <- usecase.class(
            data,
            distance_matrix,
            iterations,
            baseline_iterations
        )
    } else if (usecase == "variables") {
        result <- usecase.variable(
            data,
            distance_method,
            iterations,
            baseline_iterations
        )
    } else if (usecase == "all") {
        result <- usecase.all(
            data,
            distance_method,
            iterations,
            baseline_iterations
        )
    } else {
        stop("Unknown usecase. Must be one of following: " +
             "single, classes, variables or all.")
    }

    return(result)
}

# ##
#' Usecase \code{variable} runs the algorithm for each variable separately.
#'
#' @param data Data
#' @param distance_method Distance method
#' @param iterations Number of iterations
#' @param baseline_iterations Number of baseline iterations
#' @return TBD
#' @examples
#' #TBD
# ##
usecase.variable <- function(
    data,
    distance_method = "euclidean",
    iterations = 10,
    baseline_iterations = 50
) {

    variables <- ncol(data) - 1
    result <- matrix(nrow = variables, ncol = iterations)
    baselines <- vector(mode = "numeric")
    collection_vector <- vector(mode = "numeric", length = nrow(data))

    for (variable in 1:variables) {

        distance_matrix <- distance(data[variable], distance_method)
        for (i in 1:iterations) {
            collection_vector <- traverse(data, distance_matrix)
            result[variable, i] <- scatter(collection_vector)
        }

        baselines <- c(baselines, baseline(data[, (variables + 1)], baseline_iterations))
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
#' @param distance_matrix A distance matrix. This is different than other
#'        usecases as it doesn't take \code{distance_method} but
#'        \code{distance_matrix}. This is because the usecase.class is reused
#'        in usecase.all.
#' @param iterations Number of iterations
#' @param baseline_iterations Number of baseline iterations
#' @param  A flag that controls whether messages are printed or not
#' @return TBD
#' @examples
#' #TBD
# ##
usecase.class <- function(
    data,
    distance_matrix,
    iterations          = 10,
    baseline_iterations  = 50
) {

    # Pick up all unique classes that the data contains
    classes <- as.numeric(unique(data[, ncol(data)]))
    if (any(classes < 0)) {
        stop("Class labels cannot be negative.")
    }

    ncols <- ncol(data) - 1
    # TODO: Don't grow in a loop :)
    result <- vector(mode = "numeric")
    baselines <- vector(mode = "numeric")

    # TODO: does this for loop maintain it's order?
    for (class in classes) {

        for (i in 1:iterations) {
            collection_vector <- as.numeric(traverse(data, distance_matrix))
            collection_vector[collection_vector != class] <- (-1)
            result <- c(result, scatter(collection_vector))
        }

         labels <- as.numeric(data[, (ncols + 1)])
         labels[labels != class] <- (-1)
         baselines <- c(baselines, baseline(labels, baseline_iterations))
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
#' @param distance_method Distance method
#' @param iterations Number of iterations
#' @param baseline_iterations Number of baseline iterations
#' @return TBD
#' @examples
#' #TBD
# ##
usecase.single <- function(
    data,
    distance_method     = "euclidean",
    iterations          = 10,
    baseline_iterations = 50
) {

    ncols <- ncol(data) - 1
    collection_vector <- vector(length = nrow(data))
    distance_matrix <- distance(data[1:ncols], distance_method)
    values <- vector(length = iterations)

    labels <- data[, ncols(data)]
    for (i in 1:iterations) {
        collection_vector <- traverse(labels, distance_matrix)
        values[i] <- scatter(collection_vector)
    }

    baseline <- baseline(collection_vector, baseline_iterations)

    return(list(
        values              = values,
        means               = (sum(values) / iterations),
        baselines           = baseline,
        collection_vector   = collection_vector
        ))
}

# ##
#' Usecase `all` runs the \code{classes} for all variables separately
#'
#' @param data Data
#' @param distance_method Distance method
#' @param iterations Number of iterations
#' @param baseline_iterations Number of baseline iterations
#' @return TBD
# ##
usecase.all <- function(
    data,
    distance_method     = "euclidean",
    iterations          = 10,
    baseline_iterations = 50
) {

    # This is the container for all results
    all <- list()
    variables <- ncol(data) - 1
    result <- matrix(nrow = variables, ncol = iterations)
    varnames <- names(data)

    # Run classwise analysis for each variable, i.e. loop over all variables and
    # run usecase classes for each.
    for (variable in 1:variables) {

        distance_matrix <- distance(data[variable], distance_method)
        result <- usecase.class(
            data,
            distance_matrix,
            iterations,
            baseline_iterations
        )

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
#' @param distance_method Distance method; must be one of following:
#'        \code{euclidean}, \code{manhattan} or \code{heom}
#' @return Returns a distance matrix.
#' @export
# ##
distance <- function(
    data,
    distance_method = "euclidean"
) {

    if (!is.data.frame(data)) {
        stop("Data must be a data frame type.")
    }

    result <- switch(
        distance_method,
        euclidean = as.matrix(dist(data, method = "euclidean")),
        manhattan = as.matrix(dist(data, method = "manhattan")),
        heom      = as.matrix(heom(data)),
        cosine    = as.matrix(cosine(data)),
        binary    = as.matrix(dist(data, method = "binary")),
        c()
        )

    if (is.null(result)) {
        stop("Invalid distance_method." +
             " Must be 'euclidean', 'manhattan' or 'heom'.")
    }

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
    changes <- n_changes(labels)

    # Calculate the theoretical maximum number of changes
    thmax <- max_changes(labels)

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
max_changes <- function(labels) {

    nmax <- 0
    max <- NULL
    n <- length(labels)
    sizes <- table(labels)

    max <- max(sizes);
    nmax <- length(as.vector(which(sizes == max)))

    # Choose the theoretical maximum. See References and further reading from
    # README.md to learn about theory of this.
    if ((nmax == 1) && (max > (n - max))) {
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
#' @param collection_vector A collection vector produced by \code{traverse}.
#' @return The number of changes in the collection vector
#' @export
# ##
n_changes <- function(collection_vector) {
    n <- length(collection_vector)
    changes <- 0
    for (i in 1:n) {
        if ((i < n) && (collection_vector[i] != collection_vector[i + 1])) {
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
#' @param dm The distance matrix calculated from \code{data}
#' @param seed
#' @return Returns the vector of labels
# ##
traverse <- function(labels, distance_matrix, seed=FALSE) {

    n <- length(labels)
    indices <- vector(mode = "numeric", length = n)

    if (seed == TRUE) {
        set.seed(1)
    }

    count <- 0
    current <- sample(1:n, size = 1)

    while (count <= n) {

        count <- count + 1
        indices[count] <- current
        if (count == n) {
            break
        }

        row <- distance_matrix[current, ]
        minima <- min(row[-c(indices)])
        minimas <- which(row == minima)
        minimas <- minimas[!(minimas %in% indices)]
        if (length(minimas) > 1) {
            if (seed == TRUE) {
                set.seed(3)
            }

            nearest <- sample(minimas, size = 1)[[1]]
        } else {
            nearest <- minimas[[1]]
        }

        current <- nearest
    }

    labels[indices]
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

    n <- length(labels)
    values <- vector(mode = "numeric", length = iterations)
    for ( i in 1:iterations) {
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

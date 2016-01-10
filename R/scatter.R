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

    if(length(classes) > 0)
        data <- data[data[, classlabel] %in% classes, ]

    # If no classlabel was provided, assume it's the last column
    if(is.null(classlabel))
        classlabel <- ncol(data)

    class_labels <- data[, classlabel]
    data[, classlabel] <- NULL

    if(length(columns) > 0)
        data <- data[, columns]

    # Move classlabel column to last.
    data$class <- class_labels

    result <- NULL
    if(usecase == "single") {
        result <- usecase.single(data, distanceMethod, iterations, nominals, baselineIterations, quiet)
    } else if(usecase == "classes") {
        # TODO: Any way to ensure classlabel is correct subscript?
        distanceMatrix = distance(data[, -classlabel], distanceMethod, nominals)
        result <- usecase.class(data, distanceMatrix, iterations, nominals, baselineIterations, quiet)
    } else if(usecase == "variables") {
        result  <- usecase.variable(data, distanceMethod, iterations, nominals, baselineIterations, quiet)
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

    for(variable in 1:variables) {
        distanceMatrix <- distance(as.data.frame(data[, variable]), distanceMethod, nominal)
        for(i in 1:iterations) {
            if(quiet == FALSE) {
                print(sprintf("Running iteration %s for variable %s...", i, variable))
            }
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
#' Usecase \code{class} runs the algorithm for each class separately.
#'
#' @param data Data
#' @param distanceMatrix A distance matrix. This is different than other
#'        usecases as it doesn't take \code{distanceMethod} but 
#'        \code{distanceMatrix}.
#' @param iterations Number of iterations
#' @param nominal Nominal attributes
#' @param baselineIterations Number of baseline iterations
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

    classes <- as.numeric(unique(data[, ncol(data)]))

    # TODO: Any other ideas to handle this? This is due to fact that R starts it's indexing from 1 instead of
    # zero and class is used also as an index in result matrix.
    if(any(classes == 0)) {
        if(quiet == FALSE) {
            print("Note, that classlabels contained zeros and all labels has been incremented by one.")
            print("That means, if you had class labels 0, 1 and 2, they're now 1, 2, 3 respectively.")                
        }
        classes <- classes + 1
    }
        
    ncols <- ncol(data) - 1

    result <- matrix(nrow = length(classes), ncol = iterations)
    baselines <- vector(mode = "numeric")

    for(class in classes) {
    
        for(i in 1:iterations) {
            if(quiet == FALSE) {
                print(sprintf("Running iteration %s for class %s...", i, class))
            }
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
#' Usecase `single`. This runs scatter algorithm for the whole dataset.
#'
#' @param data Data
#' @param distanceMethod Distance method
#' @param iterations Number of iterations
#' @param nominal Nominal attributes
#' @param baselineIterations Number of baseline iterations
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
    distanceMatrix <- distance(data[, 1:ncols], distanceMethod, nominal)
    values <- vector(length = iterations)

    for(i in 1:iterations) {
        if(quiet == FALSE) {
            print(sprintf("Running iteration %s...", i))
        }
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
#' @return TBD
# ##
usecase.all <- function(
    data,
    distanceMethod = "euclidean",
    iterations = 10,
    nominal = c(),
    baselineIterations = 50,
    quiet = FALSE) {

    variables <- ncol(data) - 1
    all <- list()
    result <- matrix(nrow = variables, ncol = iterations)
    baselines <- vector(mode = "numeric", length = baselineIterations)
    collectionVector <- vector(mode = "numeric", length = nrow(data))

    for(variable in 1:variables) {

        distanceMatrix <- distance(as.data.frame(data[, variable]), distanceMethod, nominal)

        for(i in 1:iterations) {
            if(quiet == FALSE) {
                print(sprintf("Running iteration %s for variable %s...", i, variable))
            }
            result <- usecase.class(data, distanceMatrix, 1, nominal, baselineIterations, quiet = TRUE)
            all <- c(all, result)
        }

    }

    return(all)
}

# ##
#' Distance function.
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
    nominals        = NULL) {           # Which columns are nominal; used for HEOM
    
    if(!is.data.frame(data))
        stop("Data must be a data frame type.")

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
#' @param current The current class
#' @return Returns a raw Scatter value 
# ##
scatter <- function(labels, current = NULL) {
    changes <- numChanges(labels)
    thmax <- maxChanges(labels, current)
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
#' @param current Current class; others are counterclass together
#' @return Returns maximum number of label changes within given set of labels.
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
        if((i < n) && (collectionVector[i] != collectionVector[i + 1]))
            changes <- changes + 1
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
traverse <- function(data, distanceMatrix) {

    # Assume last column is class label column, so ignore it. That's why - 1.
    ncols <- ncol(data) - 1
    nrows <- nrow(data)

    lbls <- vector(mode = "character")

    # Add column to track visited; important to do it here, NOT before counting
    # the number of columns (see above).
    data$Visited = F
    
    # Set the diagonal to NA to ignore it when finding nearest neighbour. Diagonal
    # means the distance between the case itself.
    diag(distanceMatrix) <- NA

    # Random starting point
    currentIdx <- sample(1:nrows, size = 1)

    # TODO: This seems to work now; but find out if this could be done just using
    # apply and its friends.
    while(nrow(data[data$Visited == F, ]) > 0) {

        # No more cases to visit, so stop here.
        if(all(is.na(distanceMatrix))) {
            lbls <- c(lbls, data[currentIdx, (ncols + 1)])
            break
        }

        # Save the label of the current index
        lbls <- c(lbls, data[currentIdx, (ncols + 1)])

        # Also, set the current index visited
        data[currentIdx, "Visited"] <- T

        minima <- min(distanceMatrix[currentIdx, ], na.rm = T)
        minimas <- which(distanceMatrix[currentIdx, ] == minima)

        distanceMatrix[currentIdx, ] <- NA
        distanceMatrix[, currentIdx] <- NA

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
#' Computes the statistical baseline
#'
#' Computes a statistical baseline running the algorithm multiple times and
#' calculating the mean of those runs.
#' @param labels Vector of all labels contained in the dataset
#' @param iterations Number of baseline iterations
#' @return Returns mean of iteration
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

# ##
#' Calculates separation power
#'
#' @param z Baseline
#' @param s Raw Scatter value
#' @return Returns the difference between baseline and raw scatter value.
# TODO: Is this really needed?
# ##
spower <- function(z, s) {

    return(z - s)
}
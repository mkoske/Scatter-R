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
scatter <- function(df, distmethod) {

    if(!is.data.frame(df)) {
        # Some error trigger; how?
    }

    lbls <- traverse(df)
}

# Traverse the dataset using nearest neighbour method, recording label changes
# as we go.
#
# Returns the vector of labels
# ##
traverse <- function(df) {

    labels <- vector()
    # Assume last column is class label column
    ncols <- ncol(df) - 1
    
    # Create new column to keep track, which rows are not yet visited
    df$Visited <- F

    # TODO: is there a way to extend dist function to handle other methods too?
    distances <- as.matrix(dist(df[, 1:ncols], method = "euclidean"))

    # Choose a random index for starting point
    currentIdx <- sample(1:nrow(df), 1)
    while(nrows(df[df$Visited == F]) > 0) {

	current <- df[currentIdx, ]
	df[currentIdx, ]$Visited <- T

	# TODO: Is there a better way? Is this just an ugly hack? This works as
	#       follows. It takes number of columns, removes current from among
	#       those and then takes the minima. This way the distance between
	#       itself - which is zero - is not taken into account.
	idx <- 1:ncol(distances)
	closest <- min(dist[currentIdx, idx[-currentIdx]])
	labels <- c(labels, closest[ncols - 1]) 
    }

    labels
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

    len <- length(labs)
    for(idx in 1:len) {
        if((idx < len) && (labs[idx] != labs[idx + 1]))
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

    # `w` is the theoretical maxima; initialize it as zero
    w <- 0

    # Sizes of classes
    sizes <- table(classes)

    # Maxima; this returns only one result, even if there are multiple
    maxima <- max(sizes);

    # This is a special case, where there are multiple maximas; in that case,
    # the theoretical maxima is number of classes minus one.
    if(length(as.vector(which(sizes == maxima))) > 1) {
        w <- length(classes) - 1
    }

    # TODO: Handle other situations as well

    w
}

# ##
# ##
# Computes a statistical baseline running the algorithm multiple times and
# calculating the mean of those runs.
#
# Returns mean scatter value
# ##
baseline <- function() {

}



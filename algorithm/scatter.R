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

    # Assume last column is class label column
    ncols <- ncol(df) - 1
    df[, "Visited"] <- F

    # TODO: is there a way to extend dist function to handle other methods too?
    dist <- dist(df[, 1:ncols], method = "euclidean")

    # TODO: Pick randomly a starting point
    while(nrows(df[df$Visited == F]) > 0) {

    }

}

# ##
# Compute the number of label changes
#
# Returns the number of label changes
# ##
lblchanges <- function(lbls) {

    if(!is.data.frame(lbls)) {
        # Trigger some error; how?
    }
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



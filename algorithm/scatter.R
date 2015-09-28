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

    # Assume last column is class label column
    ncols <- ncol(df) - 1

    # TODO: is there a way to extend dist function to handle other methods too?
    # TODO: find out how to remove arbitary row from data frame or if using some
    #       other data structure would be more useful or easier?
    dist <- dist(df[, 1:ncols], method = "euclidean")

    while(nrows(df) > 0) {

    }

}

# Traverse the dataset using nearest neighbour method, recording label changes
# as we go.
#
# Returns the vector of labels
# ##
traverse <- function(df) {

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



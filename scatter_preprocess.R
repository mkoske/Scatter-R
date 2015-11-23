
require(DiscriMiner)
scatpp <- new.env(parent=.GlobalEnv)

# Binarize *binarized columns of *df and scale to range 0..1 *scaled columns of *df
scatpp$preprocess  <- function(df, binarized=NULL, scaled=NULL, na.action=NULL) {
	
	# Test the df argument
	if(is.null(df)) return(df)
	if(is.null(binarized) && is.null(scaled)) return(df)

	# TODO Handle missing values
	## Remove rows where classvar is missing
	## Perform selected handling on missing values

	# Subset df for different types of preprocessing
	bin.df <- df[binarized]  # Data frame with columns to be binarized
	sca.df <- df[scaled]     # Data frame with columns to be scaled
	binarized <- colnames(bin.df) # To make sure vectors contain colnames instead of indexes
	scaled    <- colnames(sca.df) # To make sure vectors contain colnames instead of indexes
	nop.df <- df[setdiff(colnames(df),union(binarized,scaled))] # Data frame with columns not preprocessed

	# Preprocess df subsets
	bin.df <- scatpp$binarize.df(bin.df)  # Binarize cols selected for binarization
	sca.df <- scatpp$unitmap.df(sca.df)   # Scale cols selected for scaling

	# Combine subsets, order by column names and return dataframe preprocessed for Scatter algorithm
	mrg.df <- scatpp$appendMerge(scatpp$appendMerge(nop.df, sca.df), bin.df) # Combine three dataframes
	ord.df <- mrg.df[,order(names(mrg.df))]  # Order columns by name
	return(ord.df)
}

# Merge two dataframes, column by column, *df2 coming after *df1
scatpp$appendMerge <- function(df1, df2)
{

	if(ncol(df1)==0 && ncol(df2)==0) return(df1) # FIXME throw exception
	if(ncol(df1)==0) return(df2)
	if(ncol(df2)==0) return(df1)

	df_new <- df1
	for(ci in 1:ncol(df2)) {
		df_new[,ncol(df_new)+1] <- df2[,ci]
		colnames(df_new)[ncol(df_new)] <- colnames(df2)[ci]
	}
	return(df_new)
}

# Binarize all columns of *df:
# For each column, create as many new columns 
#  as there are levels to the original column.
# Returned dataframe will have binominal representation 
#  of original dataframe.
scatpp$binarize.df   <- function(df) {
	if(is.null(df)) return
	if(ncol(df)==0) return(df)

	fdf <- as.data.frame((sapply(df, as.factor))) # Mark all columns as factors
	bdf <- binarize(fdf) # Call DiscriMiner::binarize to binarize all columns
	colnames(bdf) <- scatpp$binNames(fdf) # "Binarize" column names of binarized df
	return(bdf)
}

# Return the names for binarized attributes
# In:  A dataframe with columns that will be binarized
# Out: A vector with column names for binarized variable columns
scatpp$binNames    <- function(df) {
	return(unlist(Map(paste, colnames(df), 
					lapply(df, levels), 
					MoreArgs= list(sep=":")
				),
				use.names=FALSE))
}


# Scale data frame to range 0..1
# In:  A dataframe with numeric attributes only
# Out: A dataframe with each column linearly scaled to range 0..1
scatpp$unitmap.df <- function(df) 
{
	if(is.null(df)) return
	if(ncol(df)==0) return(df)
	# df <- as.data.frame(sapply(df, as.numeric))
	# str(df)

	return(as.data.frame(
		lapply(df, function(x) { 
			(x - min (x, na.rm = TRUE)) / (max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
		})
	)) 
}


# Scatter-R data preprocessing functions

require(DiscriMiner)
scatpp <- new.env(parent=.GlobalEnv)

# Preprocesses df to correct format for Scatter function
#  classVar will not be processed at all
scatpp$preprocess  <- function(df, classVar, nominalVars=NULL, remove_NA=TRUE)
{
	# TODO test if column names are unique
	if(!is.element(classVar, colnames(df))) stop # FIXME throw exception
	classVarData <- df[classVar]
	df[classVar] <- NULL
	sca_df <- scatpp$scaled.df(df)  # Select numeric columns and unitmap
	bin_df <- scatpp$binary.df(df)  # Select nominal columns and binarize
	mrg_df <- scatpp$appendMerge(sca_df, bin_df)  # Merge unitmapped and binarized columns
	ord_df <- mrg_df[,order(names(mrg_df))]  # Order columns by name
	ord_df[classVar] <- classVarData
	return(ord_df)
}

# Merges two dataframes, column by column, df2 coming after df1
scatpp$appendMerge <- function(df1, df2)
{

	if(ncol(df1)==0 && ncol(df2)==0) stop # FIXME throw exception
	if(ncol(df1)==0) return(df2)
	if(ncol(df2)==0) return(df1)

	df_new <- df1
	for(ci in 1:ncol(df2)) {
		df_new[,ncol(df_new)+1] <- df2[,ci]
		colnames(df_new)[ncol(df_new)] <- colnames(df2)[ci]
	}
	return(df_new)
}

# Return a subset of df, numeric columns only, linearly scaled to range 0..1
scatpp$scaled.df  <- function(df)
{
	df <- scatpp$numeric.df(df)
	if(ncol(df)==0) return(df)  # If processed dataset has 0 numeric columns don't try to unitmap
	return(scatpp$unit_map(df))
}

# Return a subset of df, nominal columns only, values and colnames binarized 
scatpp$binary.df  <- function(df)
{
	ndf <- scatpp$nominal.df(df)
	if(ncol(ndf)==0) return(ndf) # If processed dataset has 0 nominal columns don't try to binarize
	bdf <- binarize(ndf)
	colnames(bdf) <- scatpp$binNames(ndf)
	return(as.data.frame(bdf))
}

# Return a dataframe containing nominal columns only
# In:  A dataframe
# Out: A subset of original dataframe with nominal columns only
scatpp$nominal.df <- function(df)
{
	nominalCols <- scatpp$nominalCols(df)
	return(df[,which(nominalCols)])
}

# Return a dataframe containing numeric columns only
# In:  A dataframe
# Out: A subset of original dataframe with numeric columns only
scatpp$numeric.df <- function(df)
{
	numericCols <- scatpp$numericCols(df)
	return(df[,which(numericCols)])
}

# Return a logical vector indicating if a column is nominal or not
scatpp$nominalCols  <- function(df) 
{
	c <- sapply(df, is.factor)
	# if(!is.null(additional)) c[additional] <- TRUE
	return(c)
}

# Return a logical vector indicating if a column is numeric or not
scatpp$numericCols <- function(df, excluded=NULL)
{
	f = sapply(df, is.factor)
	n = !f
	# if(!is.null(excluded)) n[excluded] <- FALSE
	return(n)
}

# Return the names for binarized attributes
# In:  A dataframe with nominal attributes only
# Out: A vector with column names for binarized variable columns
scatpp$binNames <- function(fdf) 
{
	vv = NULL
	for(ci in 1:ncol(fdf)) 
	{
		vv = append(vv, paste(colnames(fdf[ci]), levels(fdf[,ci]), sep=":"))
	}
	return(vv)
}

# Scale data frame to range 0..1
# In:  A dataframe with numeric attributes only
# Out: A dataframe with each column linearly scaled to range 0..1
scatpp$unit_map <- function(data) 
{
	return(as.data.frame(
		lapply(data, function(x) { 
			(x - min (x, na.rm = TRUE)) / (max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
		})
	)) 
}

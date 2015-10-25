# Scatter-R data preprocessing functions

require(DiscriMiner)
scatpp <- new.env(parent=.GlobalEnv)


scatpp$preprocess  <- function(df, classVar, nominalVars=NULL, remove_NA=TRUE)
{
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


scatpp$scaled.df  <- function(df)
{
	df <- scatpp$numeric.df(df)
	if(ncol(df)==0) return(df)  # If processed dataset has 0 numeric columns don't try to unitmap
	return(scatpp$unit_map(df))
}

scatpp$binary.df  <- function(df)
{
	ndf <- scatpp$nominal.df(df)
	if(ncol(ndf)==0) return(ndf) # If processed dataset has 0 nominal columns don't try to binarize
	bdf <- binarize(ndf)
	colnames(bdf) <- scatpp$binNames(ndf)
	return(as.data.frame(bdf))
}

scatpp$nominal.df <- function(df)
{
	nominalCols <- scatpp$nominalCols(df)
	return(df[,which(nominalCols)])
}

scatpp$numeric.df <- function(df)
{
	numericCols <- scatpp$numericCols(df)
	return(df[,which(numericCols)])
}


scatpp$nominalCols  <- function(df) 
{
	c <- sapply(df, is.factor)
	# if(!is.null(additional)) c[additional] <- TRUE
	return(c)
}

scatpp$numericCols <- function(df, excluded=NULL)
{
	f = sapply(df, is.factor)
	n = !f
	# if(!is.null(excluded)) n[excluded] <- FALSE
	return(n)
}


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
scatpp$unit_map <- function(data) 
{
	return(as.data.frame(
		lapply(data, function(x) { 
			(x - min (x, na.rm = TRUE)) / (max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
		})
	)) 
}

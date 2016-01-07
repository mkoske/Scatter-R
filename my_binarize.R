
my_binarize <- function(df) {

	# Check that df is a dataframe with factors only, with no missing values
	if(!is.data.frame(df)) stop("df must be a data frame")
	if(!all(sapply(df, is.factor))) stop("df must be a data frame containning only factors")
	if(!all(complete.cases(df))) stop("missing values are not allowed")

	binarizedNames    <- function(df) {
		return(unlist(Map(paste, colnames(df), 
						lapply(df, levels), 
						MoreArgs= list(sep=":")
					),
					use.names=FALSE))
	}
	
	binNames <- binarizedNames(df)
	nrows <- nrow(df)  #Number of rows in binarized dataframe
	ncols <- ncol(df)  #Number of columns in binarized dataframe
	categories <- sapply(df, nlevels)  #Number of levels for each column
	categoriesCount <- sum(categories) #Number of levels for each column summed
	
	bdf <- matrix(0, nrows, categoriesCount) #New matrix for results
	i1 <- cumsum(categories) - categories+1  #Insertion index
	i2 <- cumsum(categories)                 #Insertion index
	
	for(i in 1:ncols)
	{
		lev <- levels(df[,i])
		m   <- matrix(0, nrows, categories[i]) #Binarized matrix for single variable
		for(ii in 1:categories[i]) {
			temp <- df[,i] == lev[ii] #Rows of original df where value matches the value of new binarized column
			m[temp,ii] <- 1 #Code all matches with 1, non-matches stay 0's
		}
		df[,i1[i]:i2[i]] <- m #Insert binarized single variable matrix to dataframe
	}
	
	colnames(df) <- binNames #Add "binarized" names to binarized dataframe
	return(df)
}


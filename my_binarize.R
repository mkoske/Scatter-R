
my_binarize <- function(df) {

	# Check that df is a dataframe with factors only, with no missing values
	if(!is.data.frame(df)) stop("df must be a data frame")
	if(!all(sapply(df, is.factor))) stop("df must be a data frame containning only factors")
	if(!all(complete.cases(df))) stop("missing values are not allowed")

	nrows <- nrow(df)
	ncols <- ncol(df)
	categories <- sapply(df, nlevels)
	categoriesCount <- sum(categories)
	
	bdf <- matrix(0, nrows, categoriesCount)
	i1 <- cumsum(categories) - categories+1
	i2 <- cumsum(categories)
	
	for(i in 1:ncols)
	{
		lev <- levels(df[,i])
		m   <- matrix(0, nrows, categories[i])
		for(ii in 1:categories[i]) {
			temp <- df[,i] == lev[ii]
			m[temp,ii] <- 1
		}
		df[,i1[i]:i2[i]] <- m
	}
	
	colnames(df) <- unlist(lapply(df, levels))
	return(df)
	
}


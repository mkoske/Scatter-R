# Scatter-R / Handling of missing values

#~ # Remove rows where classvar is missing
#~ df <- df[!is.na(df[classvar]),]
#~ 
#~ # Replace missing values of vector with vector mean/median
#~ v[is.na(v)] <- mean(v, na.rm=TRUE)
#~ v[is.na(v)] <- median(v, na.rm=TRUE)

# Remove rows with missing values
na.rmRows <- function(df) {
	df <- na.omit(df)
}

# Remove rows where classvar is missing
na.rmClassvarMissing(df, classvar) {
	df <- df[!is.na(df[classvar]),]
}
na.estimateByClass   <- function(df, classvar) {
	if(!is.data.frame(df)) stop("df is not a dataframe")
	if(!(classvar %in% colnames(df))) stop("classvar not found")
	
}

na.estimateByColumn  <- function(df) {
	if(!is.data.frame(df)) stop("df is not a dataframe")
	numericCols <- sapply(df, is.numeric)
	for(i in 1:ncol(df)) {
		
		# Get column median or mode
		if(numericCols[i]) {
			m <- median(df[[i]], na.rm=TRUE)
		} else {
			m <- names(sort(-table(mem[[5]])))[1]
		}
		
		# Replace missing values with median/mode
		df[[i]][is.na(df[[i]])] <- m
	}
	return(df)
}

test.func <- function() {
	if(FALSE) { 5 } 
	
	else {-5} 
}

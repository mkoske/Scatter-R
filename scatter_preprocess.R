scatter.preprocess <- function(
	df,                      # Dataframe to be preprocessed for Scatter algorithm
	classvar,                # Name or index of column containing class label
	included.attributes=NULL,# Attributes to be included (NULL includes all)
	included.classes=NULL,   # Classes to be included (NULL includes all)
	binarized=NULL,          # Names or indices of nominal attributes/columns to be binarized (NULL binarizes all factor-type columns)
	scaled=NULL,             # Names or indices of numeric attributes/columns to be scaled (NULL scales all numeric-type columns)	
	na.action=NULL           # Action for missing values {class,column,rmrows,nothing,NULL(=nothing)}
	)

{
	# ============ PREPROCESSING FUNCTIONS ============ #
	
	# Removes unselected attributes and rows with unselected classes. NULL selects all attributes/classes
	remove_unselected_attributes_and_classes <- function(df, classvar, included.attributes=NULL, included.classes=NULL) {
	
		## Strip non-included attributes
		if(!is.null(included.attributes)) {
			df <- df[included.attributes]
		}
		
		## Strip non-included classes
		if(!is.null(included.classes)) {
			df <- df[which(df[[classvar]] %in% included.classes),]
		}
		return(df)
	}
	
	# Handles missing values according to *action argument
	handle_missing_values <- function(df, classvar, action="class") {
		
		## Removes rows where classvar is missing. 
		remove_rows_with_missing_classvar <- function(df, classvar) { 
			df[!is.na(df[classvar]),] 
		}
		
		## Replaces missing values by column median/mode for specific class;
		##  median is used for numeric columns,
		##  mode for factor-type columns.
		estimate_by_class <- function(df, classvar) {
			
			### Replaces missing values in *vars with classwise median/mode/mean
			input_class_median_or_mode <- function(vars, classes, replace.with="median") {
				for(i in 1:length(vars)) {
					if(is.na(vars[i])) {
						cls <- classes[i]
						if(replace.with=="median") vars[i] <- median(vars[classes==cls], na.rm=TRUE)
						if(replace.with=="mode")   vars[i] <- names(sort(-table(vars[classes==cls])))[1]
						if(replace.with=="mean")   vars[i] <- mean(vars[classes==cls], na.rm=TRUE)
					}
				}
				return(vars)
			}
			
			numericCols <- sapply(df, is.numeric) # identify numeric columns
			
			# For each column, replace missing values with 
			#  column median/mode for class of the missing value
			for(i in 1:ncol(df)) {
				if(numericCols[i]) {
					df[i] <- input_class_median_or_mode(df[[i]],df[[classvar]],"median")
				} else {
					df[i] <- input_class_median_or_mode(df[[i]],df[[classvar]],"mode")
				}
			}
			return(estimate_by_column(df)) # Single-case classes are without class mean: use column mean instead
		}
		
		## Replaces missing values with column median/mode.
		##  median is used for numeric columns,
		##  mode for factor-type columns.
		estimate_by_column  <- function(df) {

			numericCols <- sapply(df, is.numeric)
			for(i in 1:ncol(df)) {
				
				# Get column median or mode
				if(numericCols[i]) {
					m <- median(df[[i]], na.rm=TRUE)
				} else {
					m <- names(sort(-table(df[[i]])))[1]
				}
				
				# Replace missing values with median/mode
				df[[i]][is.na(df[[i]])] <- m
			}
			return(df)
		}
		
		# Handle missing values in df
		df <- remove_rows_with_missing_classvar(df, classvar)  # All rows with missing classvar are always removed
		if(is.null(action)) action="class" # Default to 'class' for missing value handling option
		if(action=="rmrows") df <- na.omit(df)  # Action: Remove rows (with missing values)
		if(action=="class")  df <- estimate_by_class(df, classvar) # Action: Replace missing values with class median/mode
		if(action=="column") df <- estimate_by_column(df) # Action: Replace missing values with column median/mode
		
		return(df)
		
	}
	
	
	# Binarize *binarized columns of *df and scale to range 0..1 *scaled columns of *df:
	#  Binarization means that data in columns with factor-like/nominal data
	#   is divided into several columns. A single column containing a nominal variable 
	#   with e.g. 4 different values/levels will be transformed into 4 columns,
	#   each containing binary values indicating a certain value for the variable.
	#   New variable names will be the name of the original nominal variable 
	#   concatenated with the value, separated by colon.
	#  Scaling means mapping the values of each numeric column to range 0..1,
	#   so that the smallest value in the column will be mapped to 0, the
	#   highest to 1 and the values in between mapped linearly to the range.
	
	binarize_and_scale <- function(df, binarized=NULL, scaled=NULL) {
		
		require(DiscriMiner) # Binarization requires DiscriMiner package
		
		# Merge two dataframes, column by column, *df2 coming after *df1
		appendMerge <- function(df1, df2)
		{
			# Exit if there is nothing to merge
			if(ncol(df1)==0 && ncol(df2)==0) return(df1)
			if(ncol(df1)==0) return(df2)
			if(ncol(df2)==0) return(df1)
			
			# Add the columns of *df2 after the columns of *df1
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
		binarize.df   <- function(df) {
			if(is.null(df)) return
			if(ncol(df)==0) return(df)

			fdf <- as.data.frame((sapply(df, as.factor))) # Mark all columns as factors
			bdf <- binarize(fdf) # Call DiscriMiner::binarize to binarize all columns
			colnames(bdf) <- binNames(fdf) # "Binarize" column names of binarized df
			return(bdf)
		}

		# Return the names for binarized attributes
		# In:  A dataframe with columns that will be binarized
		# Out: A vector with column names for binarized variable columns
		binNames    <- function(df) {
			return(unlist(Map(paste, colnames(df), 
							lapply(df, levels), 
							MoreArgs= list(sep=":")
						),
						use.names=FALSE))
		}


		# Scale data frame to range 0..1
		# In:  A dataframe with numeric attributes only
		# Out: A dataframe with each column linearly scaled to range 0..1
		unitmap.df <- function(df) 
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
		
		# Return df unchanged if nothing is binarized or scaled
		if(is.null(binarized) && is.null(scaled)) return(df)
		
		# Subset df for different types of preprocessing
		bin.df <- df[binarized]  # Data frame with columns to be binarized
		sca.df <- df[scaled]     # Data frame with columns to be scaled
		nop.df <- df[setdiff(colnames(df),union(binarized,scaled))] # Data frame with columns not preprocessed
		

		# Preprocess df subsets
		bin.df <- binarize.df(bin.df)  # Binarize cols selected for binarization
		sca.df <- unitmap.df(sca.df)   # Scale cols selected for scaling

		# Combine subsets, order by column names and return dataframe preprocessed for Scatter algorithm
		mrg.df <- appendMerge(appendMerge(sca.df, bin.df), nop.df) # Combine three dataframes
 		# ord.df <- mrg.df[,order(names(mrg.df))]  # Order columns by name
		return(mrg.df)
		
	}
	
	# ============ TEST FUNCTIONS ==================== #
	
	# Tests an individual column selection vector.
	#  Throws error if selection vector contains a value that is not in *allowed_values.
	preprocess_selection_vector <- function(v, allowed_values) {
		
		if(is.null(v)) return(v) # No preprocessing for NULL-valued selection vectors
		vname = deparse(substitute(v)) # Place tested vector name into variable
		if(!is.vector(v)) stop(paste(vname, "is not of allowed type: try passing a vector"))
		
		# Selections for attributes/classes can be passed as a vector of names or as a vector of indices.
		#  Indices are replaced at this step with attribute/class names to simplify the processing after this step.
		if(is.numeric(v)) {
			if (!all(v %in% 1:length(allowed_values)))   stop(paste("non-existing indices selected in",vname)) # Test that indices are in appropriate range
			v <- allowed_values[v]  # Replace column indices with column names
		} else {
			if (!all(v %in% allowed_values)) stop(paste("non-existing names selected in", vname)) # Test that vector contains only names that are in allowed values.
		}
		return(v)
	}
	
	
	# ============ TESTING INPUTS ================== #
	
	if(!is.data.frame(df)) stop("df is not a dataframe")
	if(ncol(df) < 2)       stop("df has less than two columns")
	if(nrow(df) < 2)       stop("df has less than two rows")
	
	
	if(is.numeric(classvar)) {
		if(!classvar %in% 1:ncol(df))   stop("non-existing classvar index selected")
		classvar <- colnames(df)[classvar]  # Use classvar name instead of index
	} else {
		if (!classvar %in% colnames(df)) stop("non-existing classvar name selected")
	}
	
	# Replace missing values with sensible defaults
	if(!hasArg(included.attributes)) included.attributes <- colnames(df) # If included attributes are not selected, use all
	if(!hasArg(included.classes))    included.classes    <- levels(as.factor(df[[classvar]])) # If included classes are not selected, use all
	if(!hasArg(binarized))  binarized <- included.attributes[sapply(df[included.attributes], is.factor)  & included.attributes != classvar] # If binarized columns are not selected, binarize all factor-type columns of those that are selected (not including column with classvar)
	if(!hasArg(scaled))     scaled    <- included.attributes[sapply(df[included.attributes], is.numeric) & included.attributes != classvar] # If scaled columns are not selected, scale all numeric columns of those that are selected (not including column with classvar)
	if(!hasArg(na.action))  na.action <- "class" # Replace missing values with appropriate class central tendency indicator if no na.action option is passed
	
	# Handle NULL valued arguments
	if(is.null(included.attributes)) stop("NULL passed for included attributes: no attributes selected")
	if(is.null(included.classes))    stop("NULL passed for included classes: no classes selected")
	if(is.null(na.action))  na.action <- "nothing" # NULL for na.action is interpreted to mean "no action". 
	
	# Test and preprocess selection vectors
	included.attributes <- preprocess_selection_vector(included.attributes, colnames(df))
	included.classes    <- preprocess_selection_vector(included.classes, levels(as.factor(df[[classvar]])))
	scaled              <- preprocess_selection_vector(scaled, colnames(df))
	binarized           <- preprocess_selection_vector(binarized, colnames(df))

	# Test that passed scaled/binarized attributes are not overlapping
	if(any(binarized %in% scaled)) stop("attributes selected for both binarization and scaling")
	
	# Add classvar to included attributes if it is not present
	if (!is.null(included.attributes)) if(!classvar %in% included.attributes) append(included.attributes, classvar) -> included.attributes
	
	# Remove classvar from binarized/scaled attributes
	binarized <- binarized[binarized != classvar]
	scaled    <- scaled[scaled != classvar]
	
	# Test that *na.action option is supported by the scatter_preprocess function
	if(!(na.action %in% c("class","column","rmrows","","nothing"))) 
		stop("na.action is not supported. Try 'class','column','rmrows','nothing'")
		
		
	# ============ PREPROCESSING ==================== #
	
	## Remove unselected attributes and classes
	df <- remove_unselected_attributes_and_classes(df, classvar, included.attributes, included.classes)
	
	## Handle missing values
	df <- handle_missing_values(df, classvar, action=na.action)
	
	## Binarize and scale selected variables
	df <- binarize_and_scale(df, binarized, scaled)
	
	## Return preprocessed dataframe
	return(df)

}

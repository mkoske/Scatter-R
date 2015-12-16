# ```scatter.preprocess``` function

## Arguments




## Purpose and general description

The ```scatter.preprocess``` function is meant to be used in conjunction with
the ```scatter``` function to transform the data contents of a 
R ```data.frame``` object to a format suitable for the *scatter* algorithm.

## Detailed description 

In a default use case of the *scatter* algorithm,
where euclidean or manhattan distance measure is used, 
the numeric columns in the used ```data.frame``` object
should be normalized to make the variables commensurate.
The ```scatter.preprocess``` function does this 
by mapping each value in numeric columns to a value in range 0 to 1, inclusive.
The smallest value in each numeric column is mapped to 0,
the greatest value is mapped to 1, 
and the rest of the values are mapped linearly to the range 0..1.
The default use case also expects nominal variables to be **binarized**.
Binarization means that data in columns with factor-like/nominal data
is divided into several columns. 
A single column containing a nominal variable 
with e.g. 4 different values/levels 
will be transformed into 4 columns,
each containing binary values indicating a certain value for the variable.
New variable names will be 
the name of the original nominal variable 
concatenated with the value, separated by colon.

The function does the following transformations 
on the contents of the data frame passed as the first argument:

- removes unselected attributes and classes
	- removes attributes (columns) that the user has not selected to include in the preprocessed dataframe
		- if the function call does not include a vector argument 
		for included attributes, all attributes are included
	- removes rows, where the value of ```classvar``` is not included in the list of included classes
		- if the function call does not included a vector argument 
		for included classes, all classes are included
- handles missing values according to the action specified in ```na.action``` argument
	- all rows where the value of ```classvar``` is missing are always removed
	- if the value of ```na.action``` argument is ```class```,
	the missing values for each column are replaced 
	by the classwise median, mode of the class the case belongs to
	if the column contains numeric values
	or by the classwise mode, mode of the class the case belongs to
	if the column contains factor-type/nominal values
	- if the value of ```na.action``` argument is ```column```,
	the missing values for each column are replaced
	by the column mean
	if the column contains numeric values
	or by the column mode
	if the column contains factor-type/nominal values
	- if the value of ```na.action``` argument is ```rmrows```,
	the rows containing missing values are removed.
	- if the value of ```na.action``` argument is ```""```,
	missing values are not replaced
	- if the value of ```na.action``` argument is NULL,
	missing values are not replaced
	- if the value of ```na.action``` argument is ```nothing```,
	missing values are not replaced
	
	




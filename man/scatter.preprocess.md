# ```scatter.preprocess``` function

Examples:

```scatter.preprocess(df, classvar="C")```  
Preprocesses the dataframe ```df``` with default preprocessing options, 
class label is indicated to be in column with name "C".
All columns and all classes are included.
All numeric columns are scaled, all factor-type columns are binarized 
(except "C", which contains the class label).
If data frame has missing values, the classwise medians or modes for the attribute
(depending on whether the column is numeric or factor-type)
are used in place of missing values.

```scatter.preprocess(df, classvar=7, included.attributes=c("A","B","X")```  
Preprocesses the dataframe ```df``` with the default preprocessing options.
Class label is indicated to be in column 7 in the dataframe.
Columns with names "A","B" and "X" and column 7 (containing classvar) 
are included and other columns are stripped.
All classes are included.
Classwise medians or modes are used in place of missing values.

```scatter.preprocess(df, "class", included.classes=c(1:4), scaled=c("height","weight"), binarized=(c("edu_level")), na.action="rmrows")```  
Preprocesses the dataframe ```df```.
Class label is in column named "class".
Only classes with indices 1,2,3 and 4 are included; rows with class label not in this set are removed.
Columns named "height" and "weight" are scaled.
Column named "edu_level" is binarized.
Rows with missing values are removed.


**Arguments**

Argument | Type | Explanation
---------|------|------------
df       | data.frame | The data frame to preprocess for use with the **scatter** algorithm
classvar | name or index of column | The column that contains the class label data in the data frame
included.attributes | vector of names or indices of columns | Columns to be included in the preprocessing, others are stripped
included.classes    | vector names or indices of classes    | The rows where ```classvar``` is in included classes are included in the preprocessing, others are stripped
binarized | vector of names or incides of columns | Indicates the columns to be binarized in preprocessing
scaled    | vector of names or incides of columns | Indicates the columns to be scaled in preprocessing
na.action | text | Action for missing values {class,column,rmrows,nothing,NULL(=nothing)} 

If argument ```included.attributes``` is not passed, all attributes are included in the preprocessed dataframe.
NULL select none of the attributes (which is not allowed as it would result in an empty dataframe).

If argument ```included.classes``` is not passed, all classes are included in the preprocessed dataframe.
NULL select none of the classes (which is not allowed as it would result in an empty dataframe).

If argument ```binarized``` is not passed, all factor-type columns (except column containing *classvar*) are binarized.
NULL selects none of the attributes for binarization.

If argument ```scaled``` is not passed, all numeric-type columns (except column containing *classvar*) are scaled.
NULL selects none of the attributes for scaling.


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
	- removes attributes (columns) that the user has not selected 
	to include in the preprocessed dataframe
		- the attributes not included in the ```included.attributes```
		vector are removed
		- if the function call does not include a vector argument 
		for included attributes, all attributes are included
	- removes rows, where the value of ```classvar``` is not included in the list of included classes
		- the rows where the value of ```classvar``` 
		is not in ```included.classes``` vector are removed
		- if the function call does not included a vector argument 
		for included classes, all classes are included
- handles the missing values 
	- all rows where the value of ```classvar``` is missing are removed
	- other missing values are handled
	according to the action specified in ```na.action``` argument
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
- scales and binarizes attributes that are selected for those operations
	- attributes in the ```scaled``` vector are linearly mapped to 
	values in the range 0 to 1, inclusive
	- attributes in the ```binarized``` vector are divided into as many
	columns as there are unique values or *levels* for the attributes
	and the value of the attribute is indicated in the new columns
	in binary terms
		- the new attribute names consist of the original attribute name,
		colon and the name of the value


	
	




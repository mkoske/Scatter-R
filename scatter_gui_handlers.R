
# Show file dialog, read data from file, update main window based on data
sgui$hand.fileOpen <- function(h, ...) {
	sgui$sfile <- gfile(
		type="open", 
		filter=list(
			"CSV files"=list(patterns="*.csv"), 
			"TXT files"=list(patterns="*.txt")
	))

#~ 	if(is.null(sgui$sfile)) return()
#~ 	if(is.na  (sgui$sfile)) return()
	
	sgui$sdata <- as.data.frame(read.csv(file=sgui$sfile))
	sgui$sub.updateWithData()
}

sgui$hand.useSeparator.colon <- function(h, ...) {
	if(is.null(sgui$sfile)) return()
	sgui$sdata <- as.data.frame(read.csv(file=sgui$sfile, sep=";"))
	sgui$sub.updateWithData()
}

sgui$hand.useSeparator.comma <- function(h, ...) {
	if(is.null(sgui$sfile)) return()
	sgui$sdata <- as.data.frame(read.csv(file=sgui$sfile, sep=","))
	sgui$sub.updateWithData()
}

sgui$hand.useSeparator.dot <- function(h, ...) {
	if(is.null(sgui$sfile)) return()
	sgui$sdata <- as.data.frame(read.csv(file=sgui$sfile, sep="."))
	sgui$sub.updateWithData()
}

sgui$hand.useSeparator.tab <- function(h, ...) {
	if(is.null(sgui$sfile)) return()
	sgui$sdata <- as.data.frame(read.csv(file=sgui$sfile, sep="\t"))
	sgui$sub.updateWithData()
}

sgui$hand.useSeparator.space <- function(h, ...) {
	if(is.null(sgui$sfile)) return()
	sgui$sdata <- as.data.frame(read.csv(file=sgui$sfile, sep=""))
	sgui$sub.updateWithData()
}

# Call this function after changing classvar or included_variables selections
sgui$sub.selectionVectorUpdate_class <- function() {

	sgui$var.classes           <- levels(as.factor(sgui$sdata[[match(sgui$var.classvar, sgui$var.allvariables)]]))
	sgui$var.selected.classes  <- sgui$var.classes
	sgui$var.attributes        <- sgui$var.allvariables[sgui$var.allvariables != sgui$var.classvar]
	sgui$var.selected.attributes <- intersect(sgui$var.attributes, sgui$var.selected.attributes)
	
	# Attributes vector has changed, do update
	sgui$sub.selectionVectorUpdate_attribute()
}

sgui$sub.selectionVectorUpdate_attribute <- function() {

	sgui$var.attributes_num    <- intersect(sgui$var.selected.attributes, sgui$var.allvariables_num)
	sgui$var.attributes_int    <- intersect(sgui$var.selected.attributes, sgui$var.allvariables_int)
	sgui$var.attributes_fac    <- intersect(sgui$var.selected.attributes, sgui$var.allvariables_fac)
	sgui$var.attributes_flv    <- intersect(sgui$var.selected.attributes, sgui$var.allvariables_flv)

	sgui$var.selected.binarized   <- intersect(sgui$var.attributes_flv, sgui$var.selected.binarized)
	sgui$var.selected.scaled      <- intersect(sgui$var.attributes_num, sgui$var.selected.scaled)
}

sgui$sub.updateWithData <- function() {
	rowCount = nrow(sgui$sdata)
	colCount = ncol(sgui$sdata)
	dataInfo = paste(sgui$sfile, " : ", rowCount, "rows with", colCount, "attributes")
	svalue(sgui$lbl_datainfo) <- dataInfo
	
	# Initialize parameter selections
	
	## Invariants
	sgui$var.allvariables     <- colnames(sgui$sdata)
	sgui$var.allvariables_num <- sgui$var.allvariables[sapply(sgui$sdata, is.numeric)]
	sgui$var.allvariables_int <- sgui$var.allvariables[sapply(sgui$sdata, is.integer)]
	sgui$var.allvariables_fac <- sgui$var.allvariables[sapply(sgui$sdata, is.factor) ]
	sgui$var.allvariables_flv <- union(sgui$var.allvariables_fac, sgui$var.allvariables_int)
	
	## Guess classvar and extract initial classes
	sgui$var.classvar      <- colnames(sgui$sdata[sgui$guessClassvar(sgui$sdata)])
	sgui$var.classes       <- levels(as.factor(sgui$sdata[[match(sgui$var.classvar, sgui$var.allvariables)]]))

	## Do class selection update for selection vectors
	sgui$sub.selectionVectorUpdate_class()
	
	## Initialize selection vectors for classes and attributes
	sgui$var.selected.classes     <- sgui$var.classes
	sgui$var.selected.attributes  <- sgui$var.attributes
	
	## Do attribute selection update for selection vectors 
	sgui$sub.selectionVectorUpdate_attribute()

	## Initialize selection vectors for binarized and scaled attributes
	sgui$var.selected.binarized   <- sgui$var.attributes_fac
	sgui$var.selected.scaled      <- sgui$var.attributes_num

	## Update view
	sgui$sub.updateSelectionView()
}



sgui$sub.updateSelectionView <- function() {
	svalue(sgui$lbl_selClassVar)  <- sgui$var.classvar
	svalue(sgui$lbl_selVariables) <- paste(sgui$var.selected.attributes, collapse=", ")
	svalue(sgui$lbl_selClasses)   <- paste(sgui$var.selected.classes,    collapse=", ")
	svalue(sgui$lbl_ppScaled)     <- paste(sgui$var.selected.scaled,     collapse=", ")
	svalue(sgui$lbl_ppBinarized)  <- paste(sgui$var.selected.binarized,  collapse=", ")
}

# Guesses what might be the index of variable containing class information in dataframe 
sgui$guessClassvar <- function(df) {
	factors  = sapply(df, is.factor )
	integers = sapply(df, is.integer)
	numerics = sapply(df, is.numeric)
	
	for(i in length(factors ):1) if(factors[i])  return(i)
	for(i in length(integers):1) if(integers[i]) return(i)
	for(i in length(numerics):1) if(numerics[i]) return(i)
}


sgui$hand.select.classvar  <- function(h, ...) {
	if(is.null(sgui$sdata)) return()
	selWindow <- gwindow(title="Select class variable")
	cont.m <- ggroup(cont=selWindow)
	cont.a <- ggroup(cont=cont.m)
	cont.b <- ggroup(cont=cont.m, use.scrollwindow=TRUE, horizontal=FALSE, expand=TRUE)
	okButton <- gbutton("OK", cont=cont.a, hand=function(h,...)
		{
			sgui$var.classvar <- svalue(radio)
			sgui$sub.selectionVectorUpdate_class()

			sgui$sub.updateSelectionView()
			dispose(selWindow)
		})
		
	radio <- gradio(sgui$var.allvariables, selected=match(sgui$var.classvar, sgui$var.allvariables), cont=cont.b)
}

sgui$hand.select.attributes <- function(h, ...) {
	if(is.null(sgui$sdata)) return()
	selWindow <- gwindow(title="Select attributes included in the calculation")
	cont.m <- ggroup(cont=selWindow)
	cont.a <- ggroup(cont=cont.m)
	cont.b <- ggroup(cont=cont.m, use.scrollwindow=TRUE, horizontal=FALSE, expand=TRUE)
	okButton <- gbutton("OK", cont=cont.a, hand=function(h,...) {
		sgui$var.selected.attributes <- svalue(cbx)
		sgui$sub.selectionVectorUpdate_attribute()
		sgui$sub.updateSelectionView()
		dispose(selWindow)
	})
	cbx <- gcheckboxgroup(sgui$var.attributes, checked=(sgui$var.attributes %in% sgui$var.selected.attributes), cont=cont.b)

}

sgui$hand.select.classes    <- function(h, ...) {
	if(is.null(sgui$sdata)) return()
	selWindow <- gwindow(title="Select classes included in the calculation")
	cont.m <- ggroup(cont=selWindow)
	cont.a <- ggroup(cont=cont.m)
	cont.b <- ggroup(cont=cont.m, use.scrollwindow=TRUE, horizontal=FALSE, expand=TRUE)
	okButton <- gbutton("OK", cont=cont.a, hand=function(h,...) {
		sgui$var.selected.classes <- svalue(cbx)
		sgui$sub.updateSelectionView()
		dispose(selWindow)
	})
	cbx <- gcheckboxgroup(sgui$var.classes, checked=(sgui$var.classes %in% sgui$var.selected.classes), cont=cont.b)
}




sgui$hand.select.binarized     <- function(h, ...) {
	if(is.null(sgui$sdata)) return()
	selWindow <- gwindow(title="Select attributes to be binarized")
	cont.m <- ggroup(cont=selWindow)
	cont.a <- ggroup(cont=cont.m)
	cont.b <- ggroup(cont=cont.m, use.scrollwindow=TRUE, horizontal=FALSE, expand=TRUE)
	okButton <- gbutton("OK", cont=cont.a, hand=function(h,...) {
		sgui$var.selected.binarized <- svalue(cbx) 
		sgui$var.selected.scaled    <- sgui$var.selected.scaled[! sgui$var.selected.scaled %in% intersect(sgui$var.selected.scaled, sgui$var.selected.binarized)]
		sgui$sub.updateSelectionView()
		dispose(selWindow)
	})
	cbx <- gcheckboxgroup(sgui$var.attributes_flv, checked=(sgui$var.attributes_flv %in% sgui$var.selected.binarized), cont=cont.b)
}



sgui$hand.select.scaled    <- function(h, ...) {
	if(is.null(sgui$sdata)) return()
	selWindow <- gwindow(title="Select numerical attributes for scaling to range 0..1")
	cont.m <- ggroup(cont=selWindow)
	cont.a <- ggroup(cont=cont.m)
	cont.b <- ggroup(cont=cont.m, use.scrollwindow=TRUE, horizontal=FALSE, expand=TRUE)
	okButton <- gbutton("OK", cont=cont.a, hand=function(h,...) {
		sgui$var.selected.scaled <- svalue(cbx)
		sgui$var.selected.binarized <- sgui$var.selected.binarized[! sgui$var.selected.binarized %in% intersect(sgui$var.selected.scaled, sgui$var.selected.binarized)]
		sgui$sub.updateSelectionView()
		dispose(selWindow)
	})
	cbx <- gcheckboxgroup(sgui$var.attributes_num, checked=(sgui$var.attributes_num %in% sgui$var.selected.scaled), cont=cont.b)

}

sgui$hand.printSelections <- function(h, ...) {
	print("==================================================")
	sgui$func.printVector("Classvar: ",   sgui$var.classvar)
	sgui$func.printVector("Classes: ",    sgui$var.selected.classes)
	sgui$func.printVector("Attributes: ", sgui$var.selected.attributes)
	sgui$func.printVector("Binarized: ",  sgui$var.selected.binarized)
	sgui$func.printVector("Scaled: ",     sgui$var.selected.scaled)
	print("--------------------------------------------------")
}

sgui$hand.calculate <- function(h, ...) {
	sgui$ppdata <- scatpp$preprocess (
		df         = sgui$sdata, 
		binarized  = sgui$var.selected.binarized, 
		scaled     = sgui$var.selected.scaled
	)

	result <- run ( 
		data       = sgui$ppdata, 
		distmethod = svalue(sgui$rdo_selectMethod),
		iterations = svalue(sgui$spn_selectIterations),
		classes    = sgui$var.selected.classes,
		columns    = sgui$var.selected.attributes
	)
		
	print(result)
						 
}

sgui$func.printVector <- function(heading, vec) {
	print(paste(heading, paste(vec, collapse=", ")))
} 

